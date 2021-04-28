{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pltis_sogrid;

{$warn 5023 off : no warning about unused units}
interface

uses
  sogrid_register, sogrid, sogrideditor, sogridcommon, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('sogrid_register', @sogrid_register.Register);
end;

initialization
  RegisterPackage('pltis_sogrid', @Register);
end.
