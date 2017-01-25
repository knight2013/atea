unit MplayerClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Dialogs, RegExpr, ExtCtrls;

const
   PROPERTY_TIMER_VALUE = 250;   //Время опроса приложения mplayer (в микросекундах)

type TMplayerClass = class(TProcess) //Описываем класс плейера как потомка от TProcess

   private
   FPlaying: boolean;     //Флаг проигрывания
   FPaused : boolean;     //Флаг паузы
   FPlayList : string;    //Имя файла с плуйлистом .pls

   FTerminalOutput: TStringList; //Переменная для принятия ответа от mplayer

   FOldFileName: string;     //Старое имя текущего трэка
   FFileName: string;        //Текущее имя трэка
   FPercentPos: string;      //Позиция трэка в процентах

   FOldTimePos: string;      //Старая позиция трэка в секундах
   FTimePos: string;         //Позиция трэка в секундах

   FTimeLen: string;         //Длина трэка в секундах
   FComment : string;        //Комментарий трэка
   FVolume  : string;        //Громкость трэка
   FPausePos: string;        //Список пауз трека в секундах через запятую

   PropTimer: TTimer;        //Таймер опроса mplayer


   FOnChangePos : TNotifyEvent;  //События изменения позиции трэка
   FOnChangeFile: TNotifyEvent;  //Событие смены текущего трэка

   procedure OnPropTimer(Sender: TObject); //Событие опроса mplayer

  public

  constructor Create(AOwner: TComponent); override; //Создание плейера
  destructor Destroy; override;                     //Уничтожение плейера
  procedure Play;       //Играть
  procedure Pause;      //Поставить на паузу
  procedure Stop;       //Остановить
  procedure Next;       //Следующий трэк
  procedure Prev;       //Предыдущий трэк
  procedure Seek(APos: integer);            //Перемотать на процент APos
  procedure SetPlaylist(AFileName: string); //Установить новый плейлист
  procedure SetVolume(AVolume: string);     //Установить громкость 0-100
  procedure Mute(AValue: boolean);          //Приглушить

  procedure SendCommand(cmd: string);           //Отправить команду mplayer
  function GetOutput: TStringLIst;              //Получить ответ от mplayer
  function GetParam(AParamName: string): string;//Считать параметр из строки

  property Playing: boolean read FPlaying write FPlaying;   //Флаг проигрывания
  property Paused: boolean read FPaused write FPaused;      //Флаг паузы
  property PlayList: string read FPlayList write FPlayList; //Имя файла с плуйлистом .pls
  property TerminalOutput: TStringList read GetOutput;      //Переменная для принятия ответа от mplayer

  property FileName: string read FFileName write FFileName;       //Текущее имя трэка
  property PercentPos: string read FPercentPos write FPercentPos; //Позиция трэка в процентах
  property TimePos: string read FTimePos write FTimePos;          //Позиция трэка в секундах
  property TimeLen: string read FTimeLen write FTimeLen;          //Длина трэка в секундах

  property Comment: string read FComment write FComment;    //Комментарий трэка
  property Volume: string read FVolume write SetVolume;     //Громкость трэка
  property PausePos: string read FPausePos write FPausePos; //Список пауз трека в секундах через запятую


  published
  property OnChangePos: TNotifyEvent read FOnChangePos write FOnChangePos;    //События изменения позиции трэка
  property OnChangeFile: TNotifyEvent read FOnChangeFile write FOnChangeFile; //Событие смены текущего трэка

  end;


implementation

//Создание плейера
constructor TMplayerClass.Create(AOwner: TComponent);
begin
   inherited;                                //Выполним все действия предка
   FTerminalOutput:= TStringList.Create;     //Объявим переменную принятия ответа от mplayer
   PropTimer := TTimer.Create(nil);          //Объявим таймер для ответов
   PropTimer.Interval:=PROPERTY_TIMER_VALUE; //Установим интервал таймера
   PropTimer.Enabled:=True;                  //Включим таймер
   PropTimer.OnTimer:=@OnPropTimer;          //Присвоим процедуру события по таймеру
   FOldFileName := '-1';                     //Старое имя, точно невозможное -1
   FOldTimePos:='-1';                        //Старая позиция, точно невозможная -1
end;

//Уничтожение плейера
destructor TMplayerClass.Destroy;
begin
   PropTimer.Enabled:=False; //Отановим таймер
   FTerminalOutput.Free;     //ВЫгрузим переменную ответов
   PropTimer.Free;           //Уничтожим таймер
   inherited;                //Уничтожим предка
end;

//Приглушить
procedure TMplayerClass.Mute(AValue: boolean);
begin
  if FRunning then   //Если идет проигрывание
    if AValue                      //Если истина
        then SendCommand('mute')   //Команда отключения звука
        else SendCommand('unmute');//Иначе включение звука
end;

//Установить громкость 0-100
procedure TMplayerClass.SetVolume(AVolume: string);
begin
  if FRunning then  //Если идет проигрывание
    begin
      SendCommand('volume '+ AVolume + ' 1');  //Команда устанвка громкости
      FVolume:=AVolume;                        //Запомним громкость
    end;
end;

//Следующий трэк
procedure TMplayerClass.Next;
begin
  if FRunning then  //Если идет проигрывание
    begin

      FPaused:=False;

      SendCommand('pt_step 1');  //Команда вызова следующего трэка

      if Assigned(FOnChangeFile) then //Если используется событие смены файла
         FOnChangeFile(Self);         //пошлем событие

      Pause;
    end;


end;

//Предыдущий трэк
procedure TMplayerClass.Prev;
begin
  if FRunning then //Если идет проигрывание
    begin

     FPaused:=False;

      SendCommand('pt_step -1');//Команда вызова предыдущего трэка

      if Assigned(FOnChangeFile) then  //Если используется событие смены файла
         FOnChangeFile(Self);          //пошлем событие

      Pause;
    end;


end;


//Перемотать на процент APos
procedure TMplayerClass.Seek(APos: integer);
begin
  if FRunning then  //Если идет проигрывание
    begin
      try
      SendCommand('seek ' + IntToStr(APos) + ' 1'); //Команда смены позиции трэка
      except
      end;


      if Assigned(FOnChangeFile) then  //Если используется событие смены файла
         FOnChangeFile(Self);          //пошлем событие

    end;

end;


//Проверим соответствие текущей позиции со списком пауз
function IsTimePosInPauseList(ATimePos, APausePositions:string): boolean;
var
  posList: TStrings;           //Список пауз
  i: integer;                  //Переменная
  fTimePos,                    //Текущая позиция
  fPause: double;              //Текущая пауза
begin

  fTimePos := StrToFloat( ATimePos );  //Позицию из строки в double

  posList:= TStringList.Create;        //Обявляем список пауз
  posList.CommaText:=APausePositions;  //Заполняем текстом

  for i := 0 to posList.Count - 1 do   //Цикл по списку пауз
   begin
     fPause := StrToFloat( posList[i]) ;  //Паузу из строки в double
     if (Abs( fPause - fTimePos ) < PROPERTY_TIMER_VALUE / 2000 )  and (fPause <> -1) then
         begin
           //Если абсолютное значение fPause - fTimePos меньше интервала опроса mplaer
           Result := True;  //Паузу нашли
           posList.Free;    //Уничтожаем список
           Exit;            //Выходим
         end;
  end;
  Result := False; //Паузу не нашли
end;

//Событие опроса mplayer
procedure TMplayerClass.OnPropTimer(Sender: TObject);
begin
  if FPaused then Exit;  //Если на паузе, то выходим

  PropTimer.Enabled:=False;       //Отключим таймер

  SendCommand('get_file_name');   //Команда получения имени трэка
  SendCommand('get_percent_pos'); //Команда позиции трэка в процентах
  SendCommand('get_time_pos');    //Команда позиции трэка в секундах
  SendCommand('get_time_length');  //Команда длины трэка в секундах

  GetOutput;                       //Прочитаем ответ mplayer

  //Расшифрем ответ
  FFileName   := GetParam('ANS_FILENAME');
  FPercentPos := GetParam('ANS_PERCENT_POSITION');
  FTimePos    := GetParam('ANS_TIME_POSITION');
  FTimeLen    := GetParam('ANS_LENGTH');

  //Если ответ ошибочен, то проинициализируем переменные
  if Trim(FPercentPos) = '' then FPercentPos := '0';
  if Trim(FTimePos) = ''    then FTimePos := '0';
  if Trim(FTimeLen) = ''    then FTimeLen := '0';
  if Trim(FPausePos) = ''   then FPausePos := '-1';

  //Если старая и новая позиция файла не совпадает
  if  FOldTimePos <> FTimePos then
    begin
      FOldTimePos := FTimePos;  //Новое становится старым

      if Assigned(FOnChangePos) then  //События смены позиции трэка
         FOnChangePos(Self);
    end;

    //Если старый и новый позиция файл не совпадает
    if  (FOldFileName<>FFileName) and (FFileName <> '') and (FOldFileName='-1') then
    begin
      FOldFileName := FFileName; //Новое становится старым

      if Assigned(FOnChangeFile) then  //События смены файла трэка
         FOnChangeFile(Self);
    end;

    //Если позиция попала в список пауз, то ставим на паузу
    try
      if IsTimePosInPauseList(FTimePos, FPausePos ) then  Pause;
    finally
    end;

    PropTimer.Enabled:=True;

end;


//Играть
procedure TMplayerClass.Play;
   var Mplayeroptions: String;
begin

  if FPaused then Pause;      //Если стоим на паузе, то снимем

  if FPlaying then Exit;      //Если играет, то выйдем

  if not FileExists( FPlayList ) then Exit;   //Если нет файла плейлиста, то выйдем

  Mplayeroptions:='-slave -quiet';   //Параметры mplayer "рабский" "тихий"
  //Запускаем mplayer с плейлистом
  CommandLine:= 'mplayer '+ Mplayeroptions +' -playlist "' + FPlayList + '"';
  Options:= [poUsePipes]; //Процесс использует pipes

  try
    Execute;  //Запускаем процесс
  except
    on E : Exception do
      ShowMessage('Error on execute Mplayer');
  end;

 if FRunning then  //Если запустили
  begin
    FPlaying:=true;  //Флаг проигрывания
    SetVolume('0');  //Звук на 0
    GetOutput;       //Читаем параметры
  end;
end;

//Остановить
procedure TMplayerClass.Stop;
begin
   if not FPlaying then Exit;  //Если не играет, то выйти
   SendCommand('stop');        //Команда остановить
   Terminate(0);               //Прервать процесс
   FPlaying:=false;            //Флаг не проигрывать
end;

//Поставить на паузу
procedure TMplayerClass.Pause;
begin

   if FPaused then              //Если пауза
   begin
      SendCommand('pause');     //Команда паузы
      FPaused := not FPaused;   //Флаг не пауза
      Exit;
   end;

 if FPlaying and not FPaused then //Если проигрывает и не пауза
   begin
    SendCommand('pause');    //Команда паузы
    FPaused := not FPaused;  //Флаг паузы
    Exit;
   end;

end;

//Установить новый плейлист
procedure TMplayerClass.SetPlayList(AFileName: string);
begin
   Stop;                          //Остановить
   FPlayList := AFileName;        //Установить файл плейлиста
   Play;                          //Проигрывать

   if Assigned(FOnChangeFile) then //Событие смены файла трэка
     FOnChangeFile(Self);

   Pause;                        //Поставить на паузу
end;

//Отправить команду mplayer
procedure TMplayerClass.SendCommand(cmd: string);
begin
   if not FRunning then Exit;   //Если не запущен, то выйти

   cmd:= cmd + LineEnding;      //Команде добавим конец строки
   try
     Input.Write ( Pointer(cmd)^, Length(cmd));   //Пишем в pipe
   except
      on E : Exception do                         //Вывод ошибки
         ShowMessage('Error in command '+ Trim(cmd) +' Mplayer');
   end;


end;

//Получить ответ от mplayer
function TMplayerClass.GetOutput: TStringList;
var
    Buffer: string;            //Буфер чтения
    BytesAvailable: DWord;     //Доступно байт
    BytesRead:LongInt;         //Прочитано байт

begin
  if not FRunning then Exit;      //Если не запущен, выйти
   FTerminalOutput.Clear;         //Очистить ответ
        try
        BytesAvailable := Output.NumBytesAvailable;  //Доступно байт
        BytesRead := 0;                              //Прочитано 0
        while BytesAvailable>0 do                    //Пока доступно больше 0
        begin
          SetLength(Buffer, BytesAvailable);                   //Установим размер буфера
          BytesRead := OutPut.Read(Buffer[1], BytesAvailable); //Считаем буфер
          FTerminalOutput.Text :=   copy(Buffer,1, BytesRead); //Запомним вывод в переменной
          BytesAvailable := Output.NumBytesAvailable;          //Проверим доступность
        end;
        except
           on E : Exception do               //Вывод ошибки
              ShowMessage('Error in get output Mplayer');
         end;

  Result := FTerminalOutput;  //Вывод результата чтения
end;

//Считать параметр из строки
function TMplayerClass.GetParam(AParamName: string): string;
var
  reg: TRegExpr;  //Регулярные выражения
begin
  Result := '';                    //Обнуляем результат
  reg:= TRegExpr.Create;           //Обявление регулярных выражений
  reg.Expression:= AParamName + '=(.*?)[\x0D\x0A]';  //Установим запрос
  if reg.Exec(FTerminalOutput.Text) then  //Если запрос найден
    Result := ReplaceRegExpr('''', reg.Match[1], '', False)    //Выведем ответ
end;

end.

