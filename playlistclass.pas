unit PlayListClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, RegExpr;

const
  DEFAULT_VOLUME     = 100;        //Громкость по умолчанию
  DEFAULT_TRACK_ITEM = '%s (%s)';  //Маска вывода трэка в список формы плейлиста


type
   TTracks = record        //Запись трэка
     ID: integer;          //Код
     Filename: string;     //Полный путь к трэку
     Comment:  string;     //Комментарий
     Volume:   string;     //Громкость
     Pause:    string;     //Пауза
     Deleted: boolean;     //Метка удаления
   end;

type TPlayListClass=class(TStringList) //Плейлист
  private
     FLastCursor  : integer; //Код текущего трэка
     FCurrFileName: string;  //Текущее имя файла трэка
     FFilename    : string;  //Файл плейлиста
     FTracks: Array of TTracks;  //Массив трэков
     FCountTracks: integer;      //Размер массива трэков

  public

     CurrTrack   : TTracks;     //Текущий трэк
     List: TStringList;         //Список трэков

     procedure SetTrack;          //Установить данные трэка
     procedure GetTrack;          //Получить данные трэка
     procedure AddTrack;          //Добавть трэк
     procedure DeleteTrack;       //Удалить трэк
     procedure GetTracksFromText; //Получить трэки из плейлиста
     procedure SetTracksToText;   //Записть трэки в плейлист


     property FileName: string read FFileName write FFileName;
     property CurrFileName: string read FCurrFileName write FCurrFileName;
     property LastCursor: integer read FLastCursor write FLastCursor;

     function GetParam(AParamName, ADefaultValue: string): string; //Получить пареметр из строки
     procedure GetList;   //Получим список трэков

     constructor Create; //Создать плейлист
     destructor Destroy;override; //Уничтожим плейлист

     procedure LoadFromFile(const AFileName: string); override; //Загрузка плейлиста
     procedure SaveToFile(const AFileName: string); override;   //Запись плейлиста

end;


implementation

//Создать плейлист
constructor TPlayListClass.Create;
begin
  FLastCursor := 0;            //Сброс кода трэка
  List := TStringList.Create;  //Объявим список трэков
  inherited;                   //Объявим предка
end;

//Уничтожим плейлист
destructor TPlayListClass.Destroy;
begin
  List.Free;  //Уничтожим список трэков
  inherited;  //Уничтожим предка
end;

//Получить трэки из плейлиста
procedure TPlayListClass.GetTracksFromText;
 var
    i: integer;              //Переменная
    reg: TRegExpr;           //Регулярное выражение

    //Подпроцедура добавление трэка
    procedure _AddTrack();
      begin
        FCountTracks := FCountTracks + 1;   //Увеличим размер массива
        SetLength( FTracks, FCountTracks);

        FTracks[FCountTracks - 1].ID := StrToInt(reg.Match[1]);       //Установим данные трэка из запросов
        FTracks[FCountTracks - 1].Filename:= GetParam('File' + reg.Match[1], '');
        FTracks[FCountTracks - 1].Comment:= GetParam('Title' + reg.Match[1], '');
        FTracks[FCountTracks - 1].Volume:= GetParam('Volume' + reg.Match[1], IntToStr(DEFAULT_VOLUME));
        FTracks[FCountTracks - 1].Pause:= GetParam('Pause' + reg.Match[1], '-1');
        FTracks[FCountTracks - 1].Deleted := False;
      end;

begin
    FCountTracks := 0;                 //Обнулим размер массива
    SetLength( FTracks, FCountTracks);

    reg:= TRegExpr.Create;             //Объявим решулярное выражение
     reg.Expression:='File(\d*?)=';    //Установим запрос на поиск файла
        if reg.Exec(Text) then         //Если нашли
          begin
             _AddTrack();              //Добавим трэк
             while reg.ExecNext do     //Пока находим следующий
               _AddTrack();            //Добавим трэк
          end;
      reg.Free;                        //Освободим регулярное выражение
end;

//Записть трэки в плейлист
procedure TPlayListClass.SetTracksToText;
 var
    i: integer;                           //Переменная
begin
 Clear;                                   //Очистим текст плейлиста
 Add('[playlist]');                       //Добавим заголовок
 for i := 0 to FCountTracks - 1 do        //Цикл по массиву трэков
    begin
       if not FTracks[i].Deleted then     //Если не помечен на удаление
         begin
            //Добавим трэк в текст файла плейлиста
            Add('File' + IntToStr(i+1) + '=' + FTracks[i].Filename);
            Add('Title' + IntToStr(i+1) + '=' + FTracks[i].Comment);
            Add('Volume' + IntToStr(i+1) + '=' + FTracks[i].Volume);
            Add('Pause' + IntToStr(i+1) + '=' + FTracks[i].Pause);
         end;
    end;
end;

//Загрузка плейлиста
procedure TPlayListClass.LoadFromFile(const AFileName: string);
begin
  inherited;              //Загрузим предку
  FFilename := AFileName; //Установим имя файла
  GetTracksFromText;      //Получим трэки из файла плейлиста
end;

//Запись плейлиста
procedure TPlayListClass.SaveToFile(const AFileName: string);
begin
 SetTracksToText; //Пождготовим трэки в файл плейлиста
 inherited;       //Запишем предка
end;

//Получим список трэков
procedure  TPlayListClass.GetList;
var
   i: Integer; //Переменная
begin
  List.Clear;                        //Очистим список
  for i := 0 to FCountTracks - 1 do  //Перебор массива трэков
   begin
     if not FTracks[FCountTracks  - 1 ].Deleted then //Если трэк не удален
        //Добавим в список тексты трэк по маске
        List.Add( Format(DEFAULT_TRACK_ITEM, [FTracks[i].Filename, FTracks[i].Comment,  FTracks[i].Volume, FTracks[i].Pause] ) );
   end;
end;




//Получить пареметр из строки
function TPlayListClass.GetParam(AParamName, ADefaultValue: string): string;
var
  reg: TRegExpr;            //Регулярное выражение
begin
  Result := ADefaultValue;  //Вернем значение по умолчанию

  reg:= TRegExpr.Create;    //Объявим регулярное выражение

  reg.Expression:= AParamName + '=(.*?)[\x0D\x0A]'; //Установим запрос
  if reg.Exec(Text) then           //Если запрос найден
    Result := Trim(reg.Match[1]);  //Вернем результат запроса


end;

//Установить данные трэка
procedure TPlayListClass.SetTrack;
var
   i: integer;                      //Переменная
begin
  if CurrFileName <> '' then               //Если текщий файл указан
  for i := 0 to FCountTracks - 1 do        //Цикл по массиву трэко
     begin
        if FTracks[i].Filename = CurrFileName then  //Если найден указанный файл
           FTracks[i] := CurrTrack;                 //Установим текущий трэк
     end
  else                                              //Если файл не указан
   if FLastCursor in [0..FCountTracks-1] then       //Если код попадает в границы массива
     FTracks[ FLastCursor ] := CurrTrack;           //Установим текущий трэк

end;

//Добавть трэк
procedure TPlayListClass.AddTrack;
var
   i: integer;                           //Переменная
begin

  FCountTracks := FCountTracks + 1;                  //Увеличим длину массива трэков
  SetLength(FTracks, FCountTracks);

  FTracks[FCountTracks  - 1 ].ID := FCountTracks  - 1;        //Добавим данные в массив из текущего трэка
  FTracks[FCountTracks  - 1 ].Filename := CurrTrack.Filename;
  FTracks[FCountTracks  - 1 ].Comment := CurrTrack.Comment;
  FTracks[FCountTracks  - 1 ].Pause := CurrTrack.Pause;
  FTracks[FCountTracks  - 1 ].Volume := CurrTrack.Volume;
  FTracks[FCountTracks  - 1 ].Deleted := False;

end;

//Удалить трэк
procedure TPlayListClass.DeleteTrack;
var
   i: integer;                                      //Переменная
begin
 if CurrFileName <> '' then                        //Если файл не указан
  for i := 0 to FCountTracks - 1 do                //Цикл по массиву трэков
     begin
          if FTracks[i].Filename = CurrFileName then //Если найден файл трэка
             FTracks[i].Deleted:=True;               //Пометим его удаленным
     end
 else
  if FLastCursor in [0..FCountTracks-1] then    //Если индекс попал в границы массива
    FTracks[ FLastCursor ].Deleted:=True;       //Пометим его удаленным

  SetTracksToText;      //Установим список трэков из массива
  GetTracksFromText;    //Получим список трэков из массива

end;

//Получить данные трэка
procedure TPlayListClass.GetTrack;
var
   i: integer;                                      //Переменная
begin

 if CurrFileName <> '' then                         //Если текщий файл указан
  for i := 0 to FCountTracks - 1 do                 //Цикл по массиву трэков
     begin
          if FTracks[i].Filename = CurrFileName then //Если файл трэка найден
             CurrTrack := FTracks[i];                //получим текущий трэк
     end
 else
  if FLastCursor in [0..FCountTracks-1] then   //Если индекс попал в границы массива
    CurrTrack := FTracks[ FLastCursor ];       //получим текущий трэк
end;

end.

