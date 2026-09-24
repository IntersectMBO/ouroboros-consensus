BEGIN { CONVFMT = "%.18g"; }

{
  SlotNo = $1; Microseconds = $12 + $13; Bytes = $14;

  CumuBytes        = CumuBytes        + Bytes       ;
  CumuMicroseconds = CumuMicroseconds + Microseconds;

  print SlotNo, CumuMicroseconds, CumuBytes;
}
