unit config;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, ExtCtrls;

type
  TfrmConfig = class(TForm)
    Label1: TLabel;
    edtServidor: TEdit;
    Label2: TLabel;
    edtPorta: TEdit;
    Button1: TButton;
    Button2: TButton;
    Label3: TLabel;
    edtNome: TEdit;
    Label4: TLabel;
    Shape1: TShape;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class function Execute:Word;
  end;

implementation

{$R *.dfm}

{ TForm1 }

class function TfrmConfig.Execute: Word;
Var
  frm:TfrmConfig;
  INI:TIniFile;
begin
    INI := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
    Try
      frm := TfrmConfig.Create(nil);
      frm.edtServidor.Text := INI.ReadString('CONFIG','SERVER','127.0.0.1');
      frm.edtNome.Text     := INI.ReadString('CONFIG','NAME','User');
      frm.edtPorta.Text    := IntToStr(INI.ReadInteger('CONFIG','PORT',8110));

      Result := frm.ShowModal;
      If Result = mrOK Then
      Begin
          INI.WriteString('CONFIG', 'SERVER', frm.edtServidor.Text);
          INI.WriteString('CONFIG', 'NAME',   frm.edtNome.Text);
          INI.WriteInteger('CONFIG','PORT',   StrToIntDef(frm.edtPorta.Text,8110));
      end;
    Finally
      frm.Free;
      FreeAndNil(INI);
    End;
end;

procedure TfrmConfig.Button2Click(Sender: TObject);
begin
    Self.ModalResult := mrCancel;
    Close;
end;

procedure TfrmConfig.Button1Click(Sender: TObject);
begin
    Self.ModalResult := mrOK;
end;

end.
