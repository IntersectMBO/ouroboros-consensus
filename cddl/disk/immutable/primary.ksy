meta:
  id: primary_file
  endian: be

seq:
  - id: current_version
    type: u1
    valid: 0x01

  - id: secondary_offsets
    type: u4
    repeat: eos
