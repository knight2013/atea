unit mplayerframeclass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,
  Dialogs, ComCtrls, Process, RegExpr, playlistedit, MplayerClass, PlayListClass;

type

  { TMplayerFrameClass }

  TMplayerFrameClass = class(TFrame)  //Описание класса фрейма плейера
    btnPlay: TButton;                 //Кнопка играть
    btnStop: TButton;                 //Кнопка стоп
    btnPause: TButton;                //Кнопка пауза
    btnPlaylist: TButton;             //Кнопка открытия плейлиста
    cbxMasterVolume: TCheckBox;       //Флаг включения мастер-громкости
    commLabel: TLabel;                //Метка
    btnEdit: TButton;                 //Кнопка открытия редактора трэка
    Label1: TLabel;                   //Метки
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    idLabel: TLabel;
    lenLabel: TLabel;
    nameLabel: TLabel;
    btnNext: TButton;                  //Кнопка следующего трэка
    Panel1: TPanel;                    //Панель
    pauseLabel: TLabel;                //Метка
    playlistLabel: TLabel;             //Метка
    posLabel: TLabel;                  //Метка
    btnPrev: TButton;                  //Кнопка предыдущего трэка
    procentLabel: TLabel;              //Метка
    barVolume: TTrackBar;               //Ползунок мастер-громкости
    barPosition: TTrackBar;            //Позунок позиции трэка
    volumeLabel: TLabel;               //Метка

    Mplayer : TMplayerClass;           //Плейер
    Playlist: TPlayListClass;          //Плейлист
    PlaylistForm: TForm2;              //Форма плейлиса

    procedure barPositionChange(Sender: TObject); //Событие смены позиции трэка
    procedure btnPlayClick(Sender: TObject);      //Нажали играть
    procedure btnStopClick(Sender: TObject);      //Нажали стоп
    procedure btnPauseClick(Sender: TObject);     //Нажали паузу
    procedure btnPlaylistClick(Sender: TObject);  //Открыть плейлист
    procedure cbxMasterVolumeChange(Sender: TObject); //Изменили флажок мастер-громкости
    procedure btnEditClick(Sender: TObject);      //Запуск редактора трэка
    procedure btnNextClick(Sender: TObject);      //Нажали следующий
    procedure btnPrevClick(Sender: TObject);      //Нажали предыдущий
    procedure barVolumeChange(Sender: TObject);    //Изменили мастер-громкость

  private
    { private declarations }
  public
    { public declarations }
    CurrentPath: string;          //Текущий путь
    EditorPath: string;           //Путь к редактору
    ID: string;                   //Имя плейера


    constructor Create(TheOwner: TComponent); override;  //Создать фрейм-плейер
    destructor Destroy; override;                        //Уничтожим фрейм-плейер
    procedure  AfterConstruction; override;              //После создания фрейма
    procedure OnChangePos(Sender: TObject);              //Смена позиции
    procedure OnChangeFile(Sender: TObject);             //Смена трэка
    procedure OnPlaylistClose                            //Закрытие формы плейлиста
              (Sender: TObject; var CloseAction: TCloseAction);


  end; 

implementation

{$R *.lfm}


//Открыть плейлист
procedure TMplayerFrameClass.btnPlaylistClick(Sender: TObject);
begin

   if PlaylistForm <> nil then  //Если форма плейлиста открыта
     begin
        PlaylistForm.Show;      //Поднимем ее поверх окон
        Exit;                   //Выйдем
     end;

   PlaylistForm:= TForm2.Create(self);      //Объявим форму плейлиста
   PlaylistForm.CurrentPath:=CurrentPath;   //Установим путь
   PlaylistForm.ID := ID;                   //Присвоим имя плейера
   PlaylistForm.Caption:=ID;                //Обновим заголовок формы плейлиста
   PlaylistForm.OnClose:=@OnPlaylistClose;  //Присвоим события закрытия формы плейлиста

   if FileExists( Playlist.FileName ) then  //Если файл плейлиста существует
     begin
      with PlaylistForm do
       begin
         Playlist.LoadFromFile( self.Playlist.FileName ); //Загрузим файл плейлиста
         Playlist.GetList;                                //Получим список трэков
         ListBox1.Clear;                                  //Очистим список
         ListBox1.Items.AddStrings( Playlist.List );      //Выведем список на форму
       end;
     end;

   PlaylistForm.Show;    //Покажем окно


end;

//Изменили флажок мастер-громкости
procedure TMplayerFrameClass.cbxMasterVolumeChange(Sender: TObject);
begin

  if not Mplayer.Paused then                              //Если не пауза
    begin
     if not cbxMasterVolume.Checked then                  //Если не мастер-громкость
          Mplayer.SetVolume(Playlist.CurrTrack.Volume)    //Установим громкость из плейлиста
        else
          Mplayer.SetVolume( IntToStr( barVolume.Position ) ) //Установим громкость из ползунка мастер-громкость
    end;

end;

//Запуск редактора трэка
procedure TMplayerFrameClass.btnEditClick(Sender: TObject);
 var
     Process: TProcess;                //Процесс
 begin
      Process := TProcess.Create(nil); //Объявим процесс

      //Подготовим строку запуска редактора  _FILENAME_ заменим на имя трэка
      Process.CommandLine := ReplaceRegExpr('_FILENAME_', EditorPath, '"' + Mplayer.FileName + '"', True );
      //Запуск нового процесса
      Process.Options := [poNewProcessGroup];
      try
        Process.Execute; //Запуск
      finally
        sleep(1000);    //Пауза
        Process.Free;   //Освоюдим процесс
      end;

end;

//Нажали следующий
procedure TMplayerFrameClass.btnNextClick(Sender: TObject);
begin
 Mplayer.Play;  //Плейер играть
 Mplayer.Next;  //Плейер перейти на следеющий трэк
end;

//Нажали предыдущий
procedure TMplayerFrameClass.btnPrevClick(Sender: TObject);
begin
  Mplayer.Play; //Плейер играть
  Mplayer.Prev; //Плейер перейти на предыдущий трэк
end;

//Изменили мастер-громкость
procedure TMplayerFrameClass.barVolumeChange(Sender: TObject);
begin
  if not Mplayer.Paused              //Если не пауза
     and cbxMasterVolume.Checked     //И флаг мастер-громкости
      then
       //Установим громкоть по движку мастер-громкости
       Mplayer.SetVolume( IntToStr( barVolume.Position ) );
end;

//Нажали играть
procedure TMplayerFrameClass.btnPlayClick(Sender: TObject);
begin
 Mplayer.Play;       //Плейер играет
 Mplayer.GetOutput;  //Получить ответ от плейера
end;

procedure TMplayerFrameClass.barPositionChange(Sender: TObject);
begin
 MPlayer.OnChangePos:=nil;
 if (barPosition.Position > StrToInt(MPlayer.PercentPos) ) or
    (barPosition.Position < StrToInt(MPlayer.PercentPos) )
 Then
 If MPlayer.Playing then
  begin
   MPlayer.Seek (barPosition.Position );
   MPlayer.Play;
  end;

 MPlayer.OnChangePos:=@OnChangePos;
end;


//Нажали стоп
procedure TMplayerFrameClass.btnStopClick(Sender: TObject);
begin
 Mplayer.Stop; //Остановили плейер
end;

//Нажали паузу
procedure TMplayerFrameClass.btnPauseClick(Sender: TObject);
begin
    Mplayer.Pause;  //Плейер на паузу
end;

//Смена позиции
procedure TMplayerFrameClass.OnChangePos(Sender: TObject);
var
   iProcPos: Integer;
begin

  ID := idLabel.Caption;   //Обновим метку плейера

 if Mplayer.Running then   //Если запущено
   begin
      nameLabel.Caption    := ExtractFileName( Mplayer.FileName ); //Обновим метки на форме
      nameLabel.Hint       := Mplayer.FileName;

      procentLabel.Caption := Mplayer.PercentPos;
      posLabel.Caption     := Mplayer.TimePos;
      lenLabel.Caption     := Mplayer.TimeLen;

      if not cbxMasterVolume.Checked   //Если НЕ флажок мастер-громкости
         then Mplayer.Volume := Playlist.CurrTrack.Volume       //Громкость из плейлиста
         else Mplayer.Volume := IntToStr ( barVolume.Position );//Громкость из ползунка

      iProcPos := 0;  //Текущая позиция
      try
        if PChar(Mplayer.PercentPos) <> nil then
          iProcPos := StrToInt(Mplayer.PercentPos); //Конвертация в целое
      except
         on Exception do;
      end;
      barPosition.Position := iProcPos;  //Установим позицию в ползунок

   end;


end;

//Смена трэка
procedure TMplayerFrameClass.OnChangeFile(Sender: TObject);
begin
  if Mplayer.Running then   //Если запущено
   begin

     Playlist.CurrFileName:=Mplayer.FileName; //Обновим файл плейлиста

     Playlist.GetTrack;                       //Получим данные трэка

     nameLabel.Caption  := Playlist.CurrTrack.Filename;
     commLabel.Caption := Playlist.CurrTrack.Comment;  //Обновим поля на форме
     volumeLabel.Caption := Playlist.CurrTrack.Volume;
     pauseLabel.Caption := Playlist.CurrTrack.Pause;

     Mplayer.Comment  := Playlist.CurrTrack.Comment;
     Mplayer.Volume   := Playlist.CurrTrack.Volume;
     Mplayer.PausePos := Playlist.CurrTrack.Pause;

     barPosition.Max  := 100; //Максимальная позиция 100%

   end;
end;

//Создать фрейм-плейер
constructor TMplayerFrameClass.Create(TheOwner: TComponent);
begin
  inherited;                             //Создаем предка
  Mplayer := TMplayerClass.Create(Self); //Объявим плейер
  Playlist:= TPlayListClass.Create;      //Объявим плейлист
end;

//Закрытие формы плейлиста
procedure TMplayerFrameClass.OnPlaylistClose(Sender: TObject; var CloseAction: TCloseAction);
begin
      if FileExists( PlaylistForm.Playlist.FileName ) then  //Если файл плейлиста существует
      begin
        Playlist.LoadFromFile( PlaylistForm.Playlist.FileName );   //Загрузим плейлист
        Mplayer.SetPlaylist( PlaylistForm.Playlist.FileName );     //Установим плейлист в плейер
        playlistLabel.Caption := ExtractFileName( PlaylistForm.Playlist.FileName ); //Обновим имя плейлиста
        playlistLabel.Hint    := PlaylistForm.Playlist.FileName;  //В описание полное имя плейлиста
      end;

      CloseAction:=caFree;  //Команда уничтожения формы
      PlaylistForm := nil;  //Сброс от мусора
end;

//После создания фрейма
procedure  TMplayerFrameClass.AfterConstruction;
begin
  Mplayer.OnChangePos:=@OnChangePos;   //Присвоим процедуру события смены позиции
  Mplayer.OnChangeFile:=@OnChangeFile; //Присвоим процедуру события смены трэка
end;

//Уничтожим фрейм-плейер
destructor TMplayerFrameClass.Destroy;
begin
  Mplayer.Stop;   //Остановим плейер
  inherited;      //Уничтожим предка
end;

end.

