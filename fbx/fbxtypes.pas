unit fbxtypes;

{$mode ObjFPC}{$H+}

interface

// https://code.blender.org/2013/08/fbx-binary-file-format-specification/

uses
  Classes, SysUtils;

type
  TFbxFileHeader = packed record
    Id : array [0..20] of char; // Kaydara FBX Binary
    bt1, bt2 : Byte; // 0x1A, 0x00
    Version  : UInt32;
  end;

  TFbxNode = packed record
    EndOffset       : UInt32;
    NumProperties   : UInt32;
    PropertyListLen : UInt32;
    NameLen         : UInt8;
  end;


function IsNullNode(const n : TFbxNode): Boolean;

implementation

function IsNullNode(const n : TFbxNode): Boolean;
begin
  Result := (n.EndOffset = 0)
    and (n.NumProperties = 0 )
    and (n.PropertyListLen = 0)
    and (n.NameLen = 0);
end;

end.

