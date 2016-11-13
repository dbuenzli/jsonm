
- Safe-string suport.
- Uchar.t support. At the API level only `Jsonm.error` changes.
- Build depend on topkg.
- Relicensed from BSD3 to ISC.
- Fix `Jsonm.decode` not eventually returning `End on toplevel
  decode error.
- Support for RFC 7195/ECMA-404. This means that any JSON value can
  now be codec as JSON text, in RFC 4627 (obsoleted by 7195) this
  could only be an array or an object. If your code was relying on the
  fact the first decoded lexeme was either a ````Os``` or ````Es```,
  you will need to review that.


v0.9.1 2012-08-05 Lausanne 
--------------------------

- OASIS 0.3.0 support.


v0.9.0 2012-05-05 La Forclaz (VS)
---------------------------------

First release.
