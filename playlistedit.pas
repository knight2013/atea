unit playlistedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, PlayListClass, RegExpr;

type

  { TForm2 }

  TForm2 = class(TForm)            //Класс формы плейлиста
    btnNew: TButton;               //Кнопка нового трэка
    btnDelete: TButton;            //Кнопка удаления трэка
    btnSave1: TButton;
    btnUpdate: TButton;            //Кнопка обновления трэка
    btnLoad: TButton;              //Кнопка загрузки плейлиста
    btnSave: TButton;              //Кнопка записи плейлиста
    btnGetFilename: TButton;       //Кнопка выбра трэка
    edtFilename: TEdit;            //Имя трэка
    edtComment: TEdit;             //Комментарий трэка
    edtVolume: TEdit;              //Громкость трэка
    edtPause: TEdit;               //Паузы трэка
    Label1: TLabel;                //Метки
    Label2: TLabel;
    lab1: TLabel;
    lab2: TLabel;
    ListBox1: TListBox;            //Список трэков
    Panel1: TPanel;                //Панель нижняя
    procedure btnNewClick(Sender: TObject);        //Новый трэк
    procedure btnDeleteClick(Sender: TObject);     //Удалить трэк
    procedure btnSave1Click(Sender: TObject);      //Записать плейлист
    procedure btnUpdateClick(Sender: TObject);     //Обновить трэк
    procedure btnLoadClick(Sender: TObject);       //Загрузить плейлист
    procedure btnSaveClick(Sender: TObject);       //Сохраним плейлист
    procedure btnGetFilenameClick(Sender: TObject);//Выбрать файл трэка
    procedure FormClose                            //Закрытие формы
              (Sender: TObject; var CloseAction: TCloseAction);
    procedure ListBox1Click(Sender: TObject); //Выбор трэка
  private
    { private declarations }
  public
    { public declarations }
    Playlist : TPlayListClass;    //Плейлист
    CurrentPath: string;          //Текущий путь
    ID: string;                   //Метка плейлиста
    constructor Create(TheOwner: TComponent); override; //Создание формы
  end; 

var
  Form2: TForm2;  //Обявим форму плейлиста

implementation

{$R *.lfm}

//Загрузить плейлист
procedure TForm2.btnLoadClick(Sender: TObject);
var
 OpenDialog: TOpenDialog;                 //Диалог файлов
begin
 OpenDialog := TOpenDialog.Create(self);  //Объявим диалог файлов
 OpenDialog.InitialDir := CurrentPath;    //Установим путь
 if OpenDialog.Execute then               //Если выбрали файл
   begin
    Playlist.LoadFromFile(OpenDialog.FileName);  //Загрузим плейлист
    Playlist.FileName:=OpenDialog.FileName;      //Установим имя плейлиста
    Caption:= ID + '[' + Playlist.FileName +']'; //Обновим заголовок формы
    Playlist.GetList;                            //Обновим список трэков
    ListBox1.Items.Clear;                        //Очистим список
    ListBox1.Items.AddStrings( Playlist.List );  //Добавим трэки в список
    tag := 0;                                    //Сброс метки нового
   end;                                          //Освободим диаолог файлов
 OpenDialog.Free;
end;

//Сохраним плейлист
procedure TForm2.btnSaveClick(Sender: TObject);
var
 SaveDialog: TSaveDialog;                  //Диалог файлов
begin
 SaveDialog := TSaveDialog.Create(self);  //Объявим диалог файлов
 SaveDialog.InitialDir := CurrentPath;    //Установим путь

 if SaveDialog.Execute then               //Если выбрали файл
   begin

     if FileExists( SaveDialog.FileName ) then  //Если файл существует
       //Если НЕ согласились перезаписать
      if MessageDlg('File exists! Rewrite playlist?',mtConfirmation, mbYesNo, 0) <> mrYes then
        begin
          SaveDialog.Free; //Освободим диалог файлов
          Exit;            //выйти
        end;

     Playlist.FileName:=SaveDialog.FileName;      //Установим имя плейлиста
     Playlist.SaveToFile(SaveDialog.FileName);    //Выгрузим плейлист
     Caption:= ID + '[' + Playlist.FileName +']'; //Обновим заголовок формы
     tag := 0;                                    //Сброс метки нового
   end;
 SaveDialog.Free;                                 //Освободим диалог файлов
end;

//Выбрать файл трэка
procedure TForm2.btnGetFilenameClick(Sender: TObject);
var
 OpenDialog: TOpenDialog;                  //Диалог файлов
begin
 OpenDialog := TOpenDialog.Create(self);   //Объявим диалог файлов
 OpenDialog.InitialDir := CurrentPath;     //Установим путь
 if OpenDialog.Execute then                //Если выбрали файл
   begin
     edtFilename.Text := OpenDialog.FileName; //Имя файла в текстовое поле
   end;
 OpenDialog.Free;                             //Освободим диалог файлов

end;

//Закрытие формы
procedure TForm2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 //Если файл плейлиста существует, то запишем его перед закрытием
 if FileExists( Playlist.FileName ) then
    Playlist.SaveToFile( Playlist.FileName );
end;


//Обновить трэк
procedure TForm2.btnUpdateClick(Sender: TObject);
begin
     if tag = 1 then   //Если метка нового
        begin
          //Пропишем текстовые поля в плейлист
          Playlist.CurrTrack.Filename := edtFilename.Text;
          Playlist.CurrTrack.Comment := edtComment.Text;
          Playlist.CurrTrack.Volume := edtVolume.Text;
          Playlist.CurrTrack.Pause := edtPause.Text;

          //Добавим новую запись
          Playlist.AddTrack;

          //Обновим список трэков
          Playlist.GetList;
          //Добавим в список трэк
          ListBox1.Items.Add( Format(DEFAULT_TRACK_ITEM, [ Playlist.CurrTrack.FileName, Playlist.CurrTrack.Comment, Playlist.CurrTrack.Volume, Playlist.CurrTrack.Pause] ));
        end
     else
       begin
         //Если обновление

         Playlist.LastCursor:= ListBox1.ItemIndex;        //Установим индекс трэка

         Playlist.CurrTrack.Filename := edtFilename.Text; //Обновим поля в плейлисте
         Playlist.CurrTrack.Comment := edtComment.Text;
         Playlist.CurrTrack.Volume := edtVolume.Text;
         Playlist.CurrTrack.Pause := edtPause.Text;

         Playlist.SetTrack;                               //Обновим трэк
         ListBox1.Items[ListBox1.ItemIndex] := Format(DEFAULT_TRACK_ITEM, [ Playlist.CurrTrack.FileName, Playlist.CurrTrack.Comment, Playlist.CurrTrack.Volume, Playlist.CurrTrack.Pause] );
         //Обновим данные в списке
       end;

       //Сбросим метку нового
       tag := 0;

 //Если файл плейлиста существует, то запишем его перед закрытием
   if FileExists( Playlist.FileName ) then
      Playlist.SaveToFile( Playlist.FileName );
end;

//Новый трэк
procedure TForm2.btnNewClick(Sender: TObject);
begin

     //Обнуляем текстовые поля трэка
     edtFilename.Text:='';
     edtComment.Text:='';
     edtVolume.Text:='';
     edtPause.Text:='';

     //Метка создания нового рэка
     tag := 1;

end;

//Удалить трэк
procedure TForm2.btnDeleteClick(Sender: TObject);
begin
 if ListBox1.Count > 0 then  //Если треки есть
   begin
     Playlist.LastCursor := ListBox1.ItemIndex;  //Курсор на номер в списке трэка
     Playlist.DeleteTrack;                       //Удалить трэк в плейлисте
     ListBox1.Items.Delete( ListBox1.ItemIndex );//Удалить трэк из списка

     edtFilename.Text:='';                       //Обнулим поля
     edtComment.Text:='';
     edtVolume.Text:='';
     edtPause.Text:='';

   end;
end;

//Записать плейлист
procedure TForm2.btnSave1Click(Sender: TObject);
begin
 //Если файл плейлиста существует, то запишем его перед закрытием
   if FileExists( Playlist.FileName ) then
      Playlist.SaveToFile( Playlist.FileName );
end;


//Выбор трэка
procedure TForm2.ListBox1Click(Sender: TObject);
begin
 if ListBox1.ItemIndex <> -1 then //Если трэк выбран
   begin
     Playlist.CurrFileName := '';
     Playlist.LastCursor:= ListBox1.ItemIndex; //Поиск по индексу списка
     Playlist.GetTrack;                        //Получить данные о трэке

     edtFilename.Text:=Playlist.CurrTrack.Filename; //Пропишем параметры из плейлиста
     edtComment.Text:=Playlist.CurrTrack.Comment;
     edtVolume.Text:=Playlist.CurrTrack.Volume;
     edtPause.Text:=Playlist.CurrTrack.Pause;

     tag := 0;
   end;
end;

//Создание формы
constructor TForm2.Create(TheOwner: TComponent);
begin
  inherited;     //Создаем предка
  Playlist := TPlayListClass.Create; //Объявляем плейлист
end;

end.

