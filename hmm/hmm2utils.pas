unit hmm2utils;

interface

uses
  Classes, SysUtils, hmm2agg;

const
  // this is a default game palette
  DefaultPal : THMMPalette = (
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $3F shl 2; g: $3F shl 2; b: $3F shl 2),
    (r: $3C shl 2; g: $3C shl 2; b: $3C shl 2),
    (r: $3A shl 2; g: $3A shl 2; b: $3A shl 2),
    (r: $37 shl 2; g: $37 shl 2; b: $37 shl 2),
    (r: $35 shl 2; g: $35 shl 2; b: $35 shl 2),
    (r: $32 shl 2; g: $32 shl 2; b: $32 shl 2),
    (r: $30 shl 2; g: $30 shl 2; b: $30 shl 2),
    (r: $2D shl 2; g: $2D shl 2; b: $2D shl 2),
    (r: $2B shl 2; g: $2B shl 2; b: $2B shl 2),
    (r: $29 shl 2; g: $29 shl 2; b: $29 shl 2),
    (r: $26 shl 2; g: $26 shl 2; b: $26 shl 2),
    (r: $24 shl 2; g: $24 shl 2; b: $24 shl 2),
    (r: $21 shl 2; g: $21 shl 2; b: $21 shl 2),
    (r: $1F shl 2; g: $1F shl 2; b: $1F shl 2),
    (r: $1C shl 2; g: $1C shl 2; b: $1C shl 2),
    (r: $1A shl 2; g: $1A shl 2; b: $1A shl 2),
    (r: $17 shl 2; g: $17 shl 2; b: $17 shl 2),
    (r: $15 shl 2; g: $15 shl 2; b: $15 shl 2),
    (r: $12 shl 2; g: $12 shl 2; b: $12 shl 2),
    (r: $10 shl 2; g: $10 shl 2; b: $10 shl 2),
    (r: $0E shl 2; g: $0E shl 2; b: $0E shl 2),
    (r: $0B shl 2; g: $0B shl 2; b: $0B shl 2),
    (r: $09 shl 2; g: $09 shl 2; b: $09 shl 2),
    (r: $06 shl 2; g: $06 shl 2; b: $06 shl 2),
    (r: $04 shl 2; g: $04 shl 2; b: $04 shl 2),
    (r: $01 shl 2; g: $01 shl 2; b: $01 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $3F shl 2; g: $3B shl 2; b: $37 shl 2),
    (r: $3C shl 2; g: $37 shl 2; b: $32 shl 2),
    (r: $3A shl 2; g: $34 shl 2; b: $2E shl 2),
    (r: $38 shl 2; g: $31 shl 2; b: $2A shl 2),
    (r: $36 shl 2; g: $2E shl 2; b: $26 shl 2),
    (r: $34 shl 2; g: $2A shl 2; b: $22 shl 2),
    (r: $32 shl 2; g: $28 shl 2; b: $1E shl 2),
    (r: $30 shl 2; g: $25 shl 2; b: $1B shl 2),
    (r: $2E shl 2; g: $22 shl 2; b: $18 shl 2),
    (r: $2B shl 2; g: $1F shl 2; b: $15 shl 2),
    (r: $29 shl 2; g: $1C shl 2; b: $12 shl 2),
    (r: $27 shl 2; g: $1A shl 2; b: $0F shl 2),
    (r: $25 shl 2; g: $18 shl 2; b: $0D shl 2),
    (r: $23 shl 2; g: $15 shl 2; b: $0B shl 2),
    (r: $21 shl 2; g: $13 shl 2; b: $08 shl 2),
    (r: $1F shl 2; g: $11 shl 2; b: $07 shl 2),
    (r: $1D shl 2; g: $0F shl 2; b: $05 shl 2),
    (r: $1A shl 2; g: $0D shl 2; b: $04 shl 2),
    (r: $18 shl 2; g: $0C shl 2; b: $03 shl 2),
    (r: $16 shl 2; g: $0A shl 2; b: $02 shl 2),
    (r: $14 shl 2; g: $09 shl 2; b: $01 shl 2),
    (r: $12 shl 2; g: $07 shl 2; b: $01 shl 2),
    (r: $0F shl 2; g: $06 shl 2; b: $00 shl 2),
    (r: $0D shl 2; g: $05 shl 2; b: $00 shl 2),
    (r: $0B shl 2; g: $04 shl 2; b: $00 shl 2),
    (r: $09 shl 2; g: $03 shl 2; b: $00 shl 2),
    (r: $30 shl 2; g: $33 shl 2; b: $3F shl 2),
    (r: $2B shl 2; g: $2E shl 2; b: $3C shl 2),
    (r: $26 shl 2; g: $2A shl 2; b: $3A shl 2),
    (r: $22 shl 2; g: $26 shl 2; b: $38 shl 2),
    (r: $1E shl 2; g: $22 shl 2; b: $36 shl 2),
    (r: $1A shl 2; g: $1E shl 2; b: $34 shl 2),
    (r: $16 shl 2; g: $1A shl 2; b: $31 shl 2),
    (r: $13 shl 2; g: $16 shl 2; b: $2F shl 2),
    (r: $10 shl 2; g: $13 shl 2; b: $2D shl 2),
    (r: $0D shl 2; g: $10 shl 2; b: $2B shl 2),
    (r: $0A shl 2; g: $0D shl 2; b: $29 shl 2),
    (r: $08 shl 2; g: $0C shl 2; b: $26 shl 2),
    (r: $07 shl 2; g: $0A shl 2; b: $24 shl 2),
    (r: $05 shl 2; g: $09 shl 2; b: $22 shl 2),
    (r: $04 shl 2; g: $08 shl 2; b: $20 shl 2),
    (r: $03 shl 2; g: $07 shl 2; b: $1E shl 2),
    (r: $02 shl 2; g: $06 shl 2; b: $1C shl 2),
    (r: $01 shl 2; g: $05 shl 2; b: $19 shl 2),
    (r: $01 shl 2; g: $05 shl 2; b: $17 shl 2),
    (r: $00 shl 2; g: $04 shl 2; b: $15 shl 2),
    (r: $00 shl 2; g: $03 shl 2; b: $13 shl 2),
    (r: $00 shl 2; g: $03 shl 2; b: $11 shl 2),
    (r: $2B shl 2; g: $38 shl 2; b: $27 shl 2),
    (r: $27 shl 2; g: $35 shl 2; b: $23 shl 2),
    (r: $24 shl 2; g: $33 shl 2; b: $20 shl 2),
    (r: $20 shl 2; g: $30 shl 2; b: $1C shl 2),
    (r: $1D shl 2; g: $2E shl 2; b: $19 shl 2),
    (r: $1A shl 2; g: $2C shl 2; b: $17 shl 2),
    (r: $17 shl 2; g: $29 shl 2; b: $14 shl 2),
    (r: $14 shl 2; g: $27 shl 2; b: $11 shl 2),
    (r: $12 shl 2; g: $24 shl 2; b: $0F shl 2),
    (r: $0F shl 2; g: $22 shl 2; b: $0C shl 2),
    (r: $0D shl 2; g: $1F shl 2; b: $0A shl 2),
    (r: $0B shl 2; g: $1D shl 2; b: $09 shl 2),
    (r: $09 shl 2; g: $1B shl 2; b: $07 shl 2),
    (r: $08 shl 2; g: $19 shl 2; b: $06 shl 2),
    (r: $06 shl 2; g: $17 shl 2; b: $05 shl 2),
    (r: $05 shl 2; g: $15 shl 2; b: $03 shl 2),
    (r: $03 shl 2; g: $13 shl 2; b: $02 shl 2),
    (r: $02 shl 2; g: $10 shl 2; b: $01 shl 2),
    (r: $01 shl 2; g: $0E shl 2; b: $01 shl 2),
    (r: $01 shl 2; g: $0C shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $0A shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $08 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $06 shl 2; b: $00 shl 2),
    (r: $3F shl 2; g: $3D shl 2; b: $34 shl 2),
    (r: $3E shl 2; g: $3A shl 2; b: $2B shl 2),
    (r: $3D shl 2; g: $38 shl 2; b: $23 shl 2),
    (r: $3C shl 2; g: $37 shl 2; b: $1B shl 2),
    (r: $3B shl 2; g: $35 shl 2; b: $14 shl 2),
    (r: $3A shl 2; g: $33 shl 2; b: $0D shl 2),
    (r: $39 shl 2; g: $32 shl 2; b: $05 shl 2),
    (r: $38 shl 2; g: $31 shl 2; b: $00 shl 2),
    (r: $36 shl 2; g: $2F shl 2; b: $08 shl 2),
    (r: $34 shl 2; g: $2C shl 2; b: $07 shl 2),
    (r: $32 shl 2; g: $28 shl 2; b: $06 shl 2),
    (r: $2F shl 2; g: $26 shl 2; b: $06 shl 2),
    (r: $2D shl 2; g: $23 shl 2; b: $06 shl 2),
    (r: $2A shl 2; g: $1F shl 2; b: $05 shl 2),
    (r: $27 shl 2; g: $1C shl 2; b: $04 shl 2),
    (r: $25 shl 2; g: $19 shl 2; b: $03 shl 2),
    (r: $22 shl 2; g: $16 shl 2; b: $03 shl 2),
    (r: $1F shl 2; g: $13 shl 2; b: $02 shl 2),
    (r: $1D shl 2; g: $11 shl 2; b: $02 shl 2),
    (r: $1A shl 2; g: $0F shl 2; b: $00 shl 2),
    (r: $18 shl 2; g: $0C shl 2; b: $00 shl 2),
    (r: $15 shl 2; g: $0A shl 2; b: $00 shl 2),
    (r: $13 shl 2; g: $08 shl 2; b: $00 shl 2),
    (r: $39 shl 2; g: $33 shl 2; b: $3E shl 2),
    (r: $36 shl 2; g: $2F shl 2; b: $3B shl 2),
    (r: $32 shl 2; g: $2A shl 2; b: $39 shl 2),
    (r: $30 shl 2; g: $27 shl 2; b: $36 shl 2),
    (r: $2D shl 2; g: $23 shl 2; b: $34 shl 2),
    (r: $2A shl 2; g: $1F shl 2; b: $31 shl 2),
    (r: $27 shl 2; g: $1C shl 2; b: $2F shl 2),
    (r: $24 shl 2; g: $19 shl 2; b: $2D shl 2),
    (r: $21 shl 2; g: $16 shl 2; b: $2A shl 2),
    (r: $1E shl 2; g: $13 shl 2; b: $28 shl 2),
    (r: $1C shl 2; g: $11 shl 2; b: $25 shl 2),
    (r: $19 shl 2; g: $0E shl 2; b: $23 shl 2),
    (r: $17 shl 2; g: $0C shl 2; b: $20 shl 2),
    (r: $14 shl 2; g: $0A shl 2; b: $1E shl 2),
    (r: $12 shl 2; g: $08 shl 2; b: $1B shl 2),
    (r: $10 shl 2; g: $06 shl 2; b: $19 shl 2),
    (r: $0E shl 2; g: $05 shl 2; b: $17 shl 2),
    (r: $0B shl 2; g: $02 shl 2; b: $14 shl 2),
    (r: $08 shl 2; g: $01 shl 2; b: $11 shl 2),
    (r: $06 shl 2; g: $00 shl 2; b: $0E shl 2),
    (r: $04 shl 2; g: $00 shl 2; b: $0B shl 2),
    (r: $2D shl 2; g: $3D shl 2; b: $3F shl 2),
    (r: $2A shl 2; g: $3A shl 2; b: $3C shl 2),
    (r: $28 shl 2; g: $38 shl 2; b: $3A shl 2),
    (r: $25 shl 2; g: $36 shl 2; b: $38 shl 2),
    (r: $22 shl 2; g: $33 shl 2; b: $35 shl 2),
    (r: $20 shl 2; g: $31 shl 2; b: $33 shl 2),
    (r: $1E shl 2; g: $2E shl 2; b: $31 shl 2),
    (r: $1C shl 2; g: $2C shl 2; b: $2F shl 2),
    (r: $19 shl 2; g: $2A shl 2; b: $2C shl 2),
    (r: $17 shl 2; g: $27 shl 2; b: $2A shl 2),
    (r: $16 shl 2; g: $25 shl 2; b: $28 shl 2),
    (r: $14 shl 2; g: $23 shl 2; b: $25 shl 2),
    (r: $12 shl 2; g: $20 shl 2; b: $23 shl 2),
    (r: $10 shl 2; g: $1D shl 2; b: $20 shl 2),
    (r: $0E shl 2; g: $1A shl 2; b: $1D shl 2),
    (r: $0C shl 2; g: $18 shl 2; b: $1B shl 2),
    (r: $0A shl 2; g: $15 shl 2; b: $18 shl 2),
    (r: $08 shl 2; g: $13 shl 2; b: $16 shl 2),
    (r: $07 shl 2; g: $10 shl 2; b: $13 shl 2),
    (r: $05 shl 2; g: $0E shl 2; b: $10 shl 2),
    (r: $04 shl 2; g: $0B shl 2; b: $0E shl 2),
    (r: $03 shl 2; g: $09 shl 2; b: $0B shl 2),
    (r: $02 shl 2; g: $07 shl 2; b: $09 shl 2),
    (r: $3F shl 2; g: $39 shl 2; b: $39 shl 2),
    (r: $3D shl 2; g: $34 shl 2; b: $34 shl 2),
    (r: $3C shl 2; g: $2F shl 2; b: $2F shl 2),
    (r: $3A shl 2; g: $2B shl 2; b: $2B shl 2),
    (r: $39 shl 2; g: $27 shl 2; b: $27 shl 2),
    (r: $37 shl 2; g: $23 shl 2; b: $23 shl 2),
    (r: $36 shl 2; g: $1F shl 2; b: $1F shl 2),
    (r: $34 shl 2; g: $1B shl 2; b: $1B shl 2),
    (r: $33 shl 2; g: $17 shl 2; b: $17 shl 2),
    (r: $31 shl 2; g: $14 shl 2; b: $14 shl 2),
    (r: $30 shl 2; g: $11 shl 2; b: $11 shl 2),
    (r: $2F shl 2; g: $0E shl 2; b: $0E shl 2),
    (r: $2E shl 2; g: $0B shl 2; b: $0B shl 2),
    (r: $2D shl 2; g: $09 shl 2; b: $09 shl 2),
    (r: $2A shl 2; g: $08 shl 2; b: $08 shl 2),
    (r: $27 shl 2; g: $06 shl 2; b: $06 shl 2),
    (r: $24 shl 2; g: $04 shl 2; b: $04 shl 2),
    (r: $21 shl 2; g: $03 shl 2; b: $03 shl 2),
    (r: $1E shl 2; g: $02 shl 2; b: $02 shl 2),
    (r: $1B shl 2; g: $01 shl 2; b: $01 shl 2),
    (r: $18 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $15 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $12 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $3F shl 2; g: $39 shl 2; b: $27 shl 2),
    (r: $3E shl 2; g: $36 shl 2; b: $23 shl 2),
    (r: $3D shl 2; g: $34 shl 2; b: $1F shl 2),
    (r: $3C shl 2; g: $31 shl 2; b: $1C shl 2),
    (r: $3B shl 2; g: $2E shl 2; b: $18 shl 2),
    (r: $3A shl 2; g: $2B shl 2; b: $14 shl 2),
    (r: $39 shl 2; g: $28 shl 2; b: $11 shl 2),
    (r: $38 shl 2; g: $24 shl 2; b: $0E shl 2),
    (r: $38 shl 2; g: $21 shl 2; b: $0B shl 2),
    (r: $33 shl 2; g: $1D shl 2; b: $08 shl 2),
    (r: $2E shl 2; g: $19 shl 2; b: $06 shl 2),
    (r: $29 shl 2; g: $16 shl 2; b: $04 shl 2),
    (r: $25 shl 2; g: $12 shl 2; b: $02 shl 2),
    (r: $20 shl 2; g: $0F shl 2; b: $01 shl 2),
    (r: $1B shl 2; g: $0C shl 2; b: $00 shl 2),
    (r: $17 shl 2; g: $0A shl 2; b: $00 shl 2),
    (r: $3F shl 2; g: $16 shl 2; b: $03 shl 2),
    (r: $37 shl 2; g: $0D shl 2; b: $01 shl 2),
    (r: $30 shl 2; g: $05 shl 2; b: $00 shl 2),
    (r: $29 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $3F shl 2; g: $3F shl 2; b: $00 shl 2),
    (r: $3F shl 2; g: $33 shl 2; b: $00 shl 2),
    (r: $30 shl 2; g: $23 shl 2; b: $00 shl 2),
    (r: $23 shl 2; g: $12 shl 2; b: $00 shl 2),
    (r: $29 shl 2; g: $34 shl 2; b: $00 shl 2),
    (r: $25 shl 2; g: $2F shl 2; b: $00 shl 2),
    (r: $21 shl 2; g: $2B shl 2; b: $00 shl 2),
    (r: $1E shl 2; g: $27 shl 2; b: $01 shl 2),
    (r: $1A shl 2; g: $23 shl 2; b: $01 shl 2),
    (r: $17 shl 2; g: $1E shl 2; b: $01 shl 2),
    (r: $13 shl 2; g: $1A shl 2; b: $01 shl 2),
    (r: $10 shl 2; g: $16 shl 2; b: $01 shl 2),
    (r: $0D shl 2; g: $12 shl 2; b: $01 shl 2),
    (r: $0A shl 2; g: $1E shl 2; b: $34 shl 2),
    (r: $06 shl 2; g: $1A shl 2; b: $31 shl 2),
    (r: $01 shl 2; g: $12 shl 2; b: $2D shl 2),
    (r: $00 shl 2; g: $0E shl 2; b: $2B shl 2),
    (r: $03 shl 2; g: $15 shl 2; b: $2F shl 2),
    (r: $00 shl 2; g: $0E shl 2; b: $2B shl 2),
    (r: $00 shl 2; g: $10 shl 2; b: $2D shl 2),
    (r: $21 shl 2; g: $38 shl 2; b: $3F shl 2),
    (r: $00 shl 2; g: $26 shl 2; b: $3F shl 2),
    (r: $00 shl 2; g: $14 shl 2; b: $39 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $29 shl 2),
    (r: $23 shl 2; g: $23 shl 2; b: $2F shl 2),
    (r: $1C shl 2; g: $1C shl 2; b: $27 shl 2),
    (r: $15 shl 2; g: $15 shl 2; b: $1F shl 2),
    (r: $0F shl 2; g: $0F shl 2; b: $17 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2),
    (r: $00 shl 2; g: $00 shl 2; b: $00 shl 2)
  );

procedure AGGDump(agg: THHM2AggFile);
procedure ICNDump(icn: TICNSpriteFile; DumpContent: boolean = false);
procedure ICNPixelDump(dst: TIcnPixels);

implementation

procedure AGGDump(agg: THHM2AggFile);
var
  i : integer;
begin
  writeln('count ', agg.count);
  writeln('size':10,'offset':10,'fileid':10,' name');
  for i := 0 to agg.count-1 do begin
    writeln(
      agg.info[i].size:10
      ,agg.info[i].offset:10
      ,IntToHex(agg.info[i].fileid,8):10
      ,' ', GetName(agg.name[i]));
  end;
end;

procedure ICNDump(icn: TICNSpriteFile; DumpContent: boolean);
var
  i  : integer;
  px : TIcnPixels;
begin
  writeln('count: ', icn.count);
  writeln('size:  ', icn.size);
  for i:=0 to icn.count-1 do begin
    writeln(i,' ',icn.header[i].tp,' ofs=',icn.header[i].X,'x',icn.header[i].Y,'; size=',icn.header[i].W,'x',icn.header[i].H,'; dataofs=',icn.header[i].ofs);
    if DumpContent then begin
      px := TICNPixels.Create(icn.header[i].W, icn.header[i].H, icn.header[i].tp = ICN_TYPE_MONO);
      try
        ICNDataToPixData(icn.data, icn.header[i].ofs - icn.count * sizeof(TICNSpriteHeader), px);
        ICNPixelDump(px);
        writeln;
      finally
        px.Free;
      end;
    end;
  end;
end;

procedure ICNPixelDump(dst: TIcnPixels);
var
  y,x: integer;
begin
  if dst.isMono then begin
    for y:=0 to dst.Height-1 do begin
      for x:=0 to dst.Width-1 do begin
        if dst.Lines[y][x]=0 then write('##')
        else write('..');
      end;
      writeln;
    end;
  end else begin
    for y:=0 to dst.Height-1 do begin
      for x:=0 to dst.Width-1 do begin
        if dst.Lines[y][x] = ICN_PIXEL_EMPTY then write('..')
        else if dst.Lines[y][x] = ICN_PIXEL_SHADOW then write('\\')
        else write(IntToHex(dst.Lines[y][x],2));
      end;
      writeln;
    end;
  end;
end;

end.
