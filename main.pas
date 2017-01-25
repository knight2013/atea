unit main;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, MplayerClass, PlayListClass, Process, RegExpr, mplayerframeclass;



type

  { TForm1 }

 TForm1 = class(TForm)     //Описание основной (стартовой) формы приложения
    AudioEditor: TLabel;   //Описание текстового поля
    editorEdit: TEdit;     //Текстовое поле с маской вызова аудио-редактора
    Panel1: TPanel;        //Нижняя панель

    procedure FormClose    //Событие закрытия формы
              (Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate   //Событие создания формы
              (Sender: TObject);
    procedure FormShow     //Событие формы при появлении
              (Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }

  end;

//Объявление переменных
var
  Form1: TForm1;    //Основная (стартовая) форма приложения
  AMPlayerFrame:    //Массив фреймов-плейеров
      array of TMplayerFrameClass;
  appPath: string;  //Текущая папка запуска приложения

implementation

{$R *.lfm}

{ TForm1 }
//Событие закрытия формы
procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //Освободим из память фрейм-плейеры
  AMPlayerFrame[2].Free;
  AMPlayerFrame[1].Free;
  AMPlayerFrame[0].Free;
end;

//Событие создания формы
procedure TForm1.FormCreate(Sender: TObject);
begin
  //Установим папку запуска
  appPath := ExtractFileDir(Application.ExeName) + DirectorySeparator;
end;

//Событие формы при появлении
procedure TForm1.FormShow(Sender: TObject);
begin

  Caption := 'atea';             //Заголовок окна
  SetLength(AMPlayerFrame, 3);   //Установим размер массива фрейм-плейеров

  AMPlayerFrame[2] := TMplayerFrameClass.Create(Form1);      //Объявляем фрейм-плейер
  AMPlayerFrame[2].Name:='MPlayer_3';                        //Имя
  AMPlayerFrame[2].idLabel.Caption := AMPlayerFrame[2].Name; //Метка имени
  AMPlayerFrame[2].Parent := Form1;                          //Приналлежность к основной форме
  AMPlayerFrame[2].Align:=alTop;                             //Привязка к окну наверх
  AMPlayerFrame[2].CurrentPath:=appPath;                     //Передадим текущий путь
  AMPlayerFrame[2].Show;                                     //Покажем на экране

  AMPlayerFrame[1] := TMplayerFrameClass.Create(Form1);
  AMPlayerFrame[1].Name:='MPlayer_2';
  AMPlayerFrame[1].idLabel.Caption   :=   AMPlayerFrame[1].Name;
  AMPlayerFrame[1].Parent := Form1;
  AMPlayerFrame[1].Align:=alTop;
  AMPlayerFrame[1].CurrentPath:=appPath;
  AMPlayerFrame[1].EditorPath := editorEdit.Text;
  AMPlayerFrame[1].Show;

  AMPlayerFrame[0] := TMplayerFrameClass.Create(Form1);
  AMPlayerFrame[0].Name := 'MPlayer_1';
  AMPlayerFrame[0].idLabel.Caption   :=   AMPlayerFrame[0].Name;
  AMPlayerFrame[0].Parent := Form1;
  AMPlayerFrame[0].Align:=alTop;
  AMPlayerFrame[0].CurrentPath:=appPath;
  AMPlayerFrame[0].EditorPath := editorEdit.Text;
  AMPlayerFrame[0].Show;

end;


end.

