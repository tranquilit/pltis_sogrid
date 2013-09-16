unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, sogrid, VirtualTrees;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    SOGrid1: TSOGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses tisstrings,superobject;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  //SOGrid1.JSONdata:=Memo1.Lines.Text;
  sogrid1.Data := SO(Memo1.Lines.Text);
  sogrid1.LoadData;
  sogrid1.Header.AutoFitColumns(False);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Text := SOGrid1.JSONdata;
end;

{$R *.lfm}

end.

