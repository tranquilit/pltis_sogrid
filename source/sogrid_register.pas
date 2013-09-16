{**********************************************************************
 Package pl_VirtualTreeExtra.pkg
 This unit is part of CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit sogrid_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLProc, LCLType, LMessages, LResources,
  LazIDEIntf, PropEdits, ComponentEditors,TypInfo,
  sogrid;


procedure Register;

implementation


//=============================================================
//=============================================================
//=============================================================

procedure Register;
begin

  RegisterComponents('Virtual Controls', [
                             TSOGrid
                             ]);

end;

initialization

  {$i sogrid_register.lrs}

finalization

end.

