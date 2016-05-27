0.6 (2016-05-25)
- Add a simple protocol with shutdown read,write and close

0.5 (2016-05-21)
- Bump Linux AF_HYPERV to 43
- Use one RX and one TX thread per connection
- Acquire the runtime lock before calling `uerror`
- Implement Lwt_hvsock.listen (untested)

0.4 (2016-05-12)
- Add an implementation of V1_LWT.FLOW

0.3 (2016-05-12)
- Avoid running out of Lwt_preemptive thread pool threads

0.2 (2016-05-12)
- Work around connect() blocking forever

0.1 (2016-05-12)
- Initial release
