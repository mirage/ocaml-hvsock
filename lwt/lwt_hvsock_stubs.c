/*
 * Copyright (C) 2016 Docker Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/callback.h>

CAMLprim value
stub_ba_recv(value fd, value val_buf, value val_ofs, value val_len)
{
  CAMLparam4(fd, val_buf, val_ofs, val_len);
  int ret = 0;

#ifdef WIN32
  char *data = (char*)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
  size_t c_len = Int_val(val_len);
  SOCKET s = Socket_val(fd);
  DWORD err = 0;

  caml_release_runtime_system();
  ret = recv(s, data, c_len, 0);
  if (ret == SOCKET_ERROR) err = WSAGetLastError();
  caml_acquire_runtime_system();

  if (err) {
    win32_maperr(err);
    uerror("read", Nothing);
  }
#else
  caml_failwith("AF_HYPERV only available on Windows");
#endif
  CAMLreturn(Val_int(ret));
}

CAMLprim value
stub_ba_send(value fd, value val_buf, value val_ofs, value val_len)
{
  CAMLparam4(fd, val_buf, val_ofs, val_len);
  int ret = 0;
#ifdef WIN32
  char *data = (char*)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
  size_t c_len = Int_val(val_len);
  SOCKET s = Socket_val(fd);
  DWORD err = 0;

  caml_release_runtime_system();
  ret = send(s, data, c_len, 0);
  if (ret == SOCKET_ERROR) err = WSAGetLastError();
  caml_acquire_runtime_system();

  if (err) {
    win32_maperr(err);
    uerror("read", Nothing);
  }
#else
  caml_failwith("AF_HYPERV only available on Windows");
#endif
  CAMLreturn(Val_int(ret));
}
