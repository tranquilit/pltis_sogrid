{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pltis_sogrid;

interface

uses
  sogrid_register, sogrid, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('sogrid_register', @sogrid_register.Register);
end;

initialization
  RegisterPackage('pltis_sogrid', @Register);
end.
