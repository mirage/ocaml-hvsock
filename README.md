# OCaml bindings for Hyper-V AF_VSOCK

These bindings allow Host <-> VM communication on Hyper-V systems.

Please read [the API documentation](https://mirage.github.io/ocaml-hvsock/index.html).

# Example

An address on a Hyper-V host consists of two parts:

- a VM GUID
- a well-known service GUID

First generate yourself a service GUID and add this to the registry: this is like
opening a hole in a firewall for a TCP port.

```powershell
New-Item -Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Virtualization\GuestCommunicationServices" -Name <Service GUID>
```

Second discover the GUID of the VM you wish to talk to:

```powershell
(Get-VM -Name <MY VM NAME>).Id
```

Third, run a listening server in the VM:

```bash
./hvcat -l --echo <Service GUID>
```

Finally, connect the client on the host to the VM:

```bash
./hvcat --vmid <VM GUID> <Service GUID>
```

# Limitations

Although the connections use the regular `SOCKET` APIs, current kernels don't support
calls like `select` so we must always use blocking I/O from background threads, rather
than regular asynchronous I/O. This means that the Lwt connection type has been made
opaque.

# Background

For background, see the [MSDN article on making an integration service](https://msdn.microsoft.com/en-us/virtualization/hyperv_on_windows/develop/make_mgmt_service)
