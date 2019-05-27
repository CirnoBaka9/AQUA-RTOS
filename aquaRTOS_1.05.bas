' ********************************************************************************************
'                                   A-RTOS 1.05
'
'                             by Alexandra Bernshtein
'                               vk.com/id260853760
'                                 fausto@mail.ru
' ********************************************************************************************

' отличия от версии 1.04
' добавлена возможность пересылать в качестве сообщений (служба Messages) указатели на строки
'
$nocompile

#if varexist( "NULL")
#else
   const NULL = 0                                                               ' общее
#endif
#if varexist( "FALSE")
#else
   const FALSE = NULL
#endif
#if varexist( "TRUE")
#else
   const TRUE = 1
#endif

' --- настраиваемые пользователем параметры -----

const OS_SIM = TRUE
                                                         ' на реальном железе / на симуляторе

debug off

#if OS_SIM = TRUE
   $sim
#endif

const OS_MAX_TASK = 8                                                           ' максимальное число задач в системе. каждая задача требует __12__ байт памяти
const OS_MAX_TOPIC = 16                                                         ' максимальное число тем сообщений
'const OS_MAX_CSEM = 8                                                           ' максимальное число счетных семафоров

' --- конец настраиваемых пользователем параметров -----

const OSTS_UNDEFINE = NULL                                                      ' статусы задачи
const OSTS_READY = 1
const OSTS_RUN = 2
const OSTS_DELAY = 3
const OSTS_STOP = 4
const OSTS_WAIT = 5
const OSTS_PAUSE = 6
const OSTS_RESTART = 7

const OSERR_CRITICAL = &H80                                                     ' 7th bit shows means error is critical
const OSERR_TASK_MAX_REACHED = 1
const OSERR_EVENT_MAX_REACHED = 2
const OSERR_TOPIC_MAX_REACHED = 3
const OSERR_BSEM_MAX_REACHED = 4
const OSERR_TIMEOUT = 5                                                         ' истек установленный таймаут
const OSERR_CANT_FIND_TASK = 64



dim task_ptr as word                                                            ' служебные переменные для получения точки входа в задачу
dim task_ptr_lo as byte at task_ptr overlay
dim task_ptr_hi as byte at task_ptr + 1 overlay

dim task_entry(OS_MAX_TASK) as word                                             ' точка входа задачи; служит ключом, по которым можно сослаться на задачу; ОС находит по нему внутренний ID задачи
dim task_adr_lo(OS_MAX_TASK) as byte                                            ' младший байт текущего адреса выполнения задачи
dim task_adr_hi(OS_MAX_TASK) as byte                                            ' старший
dim task_state(OS_MAX_TASK) as byte                                             ' статус задачи OSTS_*
dim task_prior(OS_MAX_TASK) as byte                                             ' приоритет задачи, 0 - высший, 15 - низший
dim task_queue(OS_MAX_TASK) as byte                                             ' очередь задач
dim task_delay(OS_MAX_TASK) as word                                             ' задержка, в тиках системного таймера
dim task_event(OS_MAX_TASK) as byte                                             ' событие, на которое подписана задача
dim task_topic(OS_MAX_TASK) as byte                                             ' тема, сообщения на которую ждет задача
dim task_bsem(OS_MAX_TASK) as word                                              ' битовые семафоры, освобождения которых ждет задача (каждый бит номер такой-то = семафор номер такой-то)

dim task_count as byte                                                          ' счетчик числа используемых в системе задач
dim task_current as byte                                                        ' id исполняемой задачи

dim user_err_trap as word                                                       ' адрес пользовательского обработчика ошибок
dim user_err_trap_lo as byte at user_err_trap overlay
dim user_err_trap_hi as byte at user_err_trap + 1 overlay

dim minprio as byte
dim minqueuidx as byte
dim task_id as byte                                                             ' id задачи
dim tim_cnt as word                                                             ' счетчик прерываний таймера
dim RR16 as byte

dim OS_TIMEOUT as boolean

dim event_count as byte                                                         ' счетчик событий, предоставляет уникальный номер события службе событий
dim topic_count as byte                                                         ' счетчик тем сообщений, предоставляет уникальный номер темы службе сообщений
dim os_message(OS_MAX_TOPIC) as word                                            ' сообщения (!word в 105!)
dim bSem_count as byte                                                          ' счетчик бинарных семафоров
dim bSem_reg as word                                                            ' регистр бинарных семафоров: каждый бит номер такой-то = семафор номер такой-то
'dim CntSemaphore(OS_MAX_CSEM) as byte                                           ' счетные семафоры
'dim CntSem_count as byte                                                        ' счетчик счетных семафоров

dim os_tmpbyte as byte
dim os_tmpword as word

const DELAY_NONE = NULL

dim isr_i as byte                                                               ' index in interrupt

disable timer0

goto RTOS_done                                                                  ' required by BASCOM compilier if used "config  submode = old"

macro MGetTaskIdByLabel
   for task_id = 1 to task_count                                                ' по адресу задачи в task_label находит ее task_id в системе или возвращает task_id := 0
      if task_entry(task_id) = task_label then exit for
   next
   if task_id > task_count then
      OS_RaiseErr OSERR_CANT_FIND_TASK or OSERR_CRITICAL
   end if
end macro

' --- прототипы функций ---
' системный сервис
declare sub OS_Init(bylabel error_trap as word)                                 ' должна быть вызвана перед запуском RTOS ; пользовательский обработчик ошибок должен быть процедурой с одним параметром типа byte, например, MyErrTrap(err_num as byte).
declare sub OS_Sheduler()                                                       ' основной диспетчер; сервис безусловной передачи управления из задачи к диспетчеру; задача, вызывавшая этот сервис, (!!НЕ - 105!!) останавливается, передавая управление диспетчеру (и другим задачам, если есть)
declare sub OS_RaiseErr(byval os_err_num as byte)                               ' вызывает пользовательский обработчик ошибки
' управление задачами
declare sub OS_InitTask(bylabel task_label as word , byval task_prio as byte)   ' любая задача, предже чем быть использована, должна быть инициализирована в системе. вызывается вне задач.
declare sub OS_Stop()                                                           ' остановить текущую задачу (АПУД). Если в дальнейшем эту задачу поставить на выполнение, она стартует с начала, а не с точки останова
declare sub OS_StopTask(bylabel task_label as word)                             ' остановить указанную задачу (АПУД). Если в дальнейшем эту задачу поставить на выполнение, она стартует с начала, а не с точки останова
declare sub OS_Pause()                                                          ' 105: приостановить текущую задачу (АУПД). Если в дальнейшем эту задачу поставить на выполнение, она стартует с точки приостанова
declare sub OS_PauseTask(bylabel task_label as word)                            ' 105: приостановить указанную задачу (АУПД). Если в дальнейшем эту задачу поставить на выполнение, она стартует с точки приостанова
declare sub OS_Resume()                                                         ' назначает указанной задаче статус "готова к исполнению", !!! не передает управление диспетчеру автоматически !!!  ; это будет сделано при вызове OS_Sheduler
declare sub OS_ResumeTask(bylabel task_label as word)                           ' назначает указанной задаче статус "готова к исполнению", !!! не передает управление диспетчеру автоматически !!!  ; это будет сделано при вызове OS_Sheduler
declare sub OS_Restart()                                                        ' перезапускает текущую задачу с начала (АУПД)

' сервис таймеров
declare sub OS_Delay(byval delay_time as word)                                  ' переводит текущую задачу в состояние "отложено на delay_time", автоматическая передача управления диспетчеру (АПУД)
declare sub OS_DelayTask(bylabel task_label as word , byval delay_time as word) ' переводит указанную задачу в состояние "отложено на delay_time", (АПУД).
' сервис событий
declare function OS_CreateEvent() as byte                                       ' возвращает уникальный идентификатор события hEvent
declare sub OS_WaitEvent(byval hEvent as byte)                                  ' переводит текущую задачу в состояние "ждать события hEvent", (АПУД)
declare sub OS_WaitEventTask(bylabel task_label as word , byval hEvent as byte) ' переводит указанную задачу состояние "ждать события hEvent", (АПУД)
declare sub OS_WaitEventTO(byval hEvent as byte , byval wTimeout as word)       ' переводит текущую задачу в состояние "ждать события hEvent" с таймаутом, выставляет флаг OS_Timeout (АПУД)
declare sub OS_SignalEvent(byval hEvent as byte)                                ' предоставляет приложению сервис, чтобы просигналить в систему о событии hEvent
' сервис сообщений
declare function OS_CreateMessage() as byte                                     ' возращает уникальный идентификатор темы hTopic, на которую будут передаваться сообщения
declare sub OS_WaitMessage(byval hTopic as byte)                                ' переводит текущую задачу в состояние "ждать сообщения по теме hTopic", (АПУД)
declare sub OS_WaitMessageTask(bylabel task_label as word , byval hTopic as byte)       ' переводит указанную задачу task_label в состояние "ждать сообщения по теме hTopic", (АПУД)     (!word in 105!)
declare sub OS_WaitMessageTO(byval hTopic as byte , byval wTimeout as word)     ' переводит текущую задачу в состояние "ждать сообщения по теме hTopic", (АПУД)
declare sub OS_SendMessage(byval hTopic as byte , byval wMessage as word)       ' предоставляет приложению сервис передать сообщение wMessage на тему hTopic; wMessage может указывать на адрес переменной-сообщения
declare function OS_GetMessage(byval hTopic as byte) as word                    ' читает посланное сообщение и уничтожает его        (!word in 105!)
declare function OS_PeekMessage(byval hTopic as byte) as word                   ' читает посланное сообщение и не уничтожает его        (!word in 105!)
declare function OS_GetMessageString(byval hTopic as byte) as string            ' возвращает строку сообщения по теме hTopic, после чего очищает его
declare function OS_PeekMessageString(byval hTopic as byte) as string           ' возвращает строку сообщения по теме hTopic, но не очищает его
' сервис бинарных семафоров
declare function OS_CreateBSemaphore() as byte                                  ' возвращает уникальный идентификатор бинарного семафора hBSem
declare sub OS_WaitBSemaphore(byval hBSem as byte)                              ' переводит текущую задачу в состояние "ждать освобождения семафора hBSem", если он "занят" (АПУД), иначе продолжать
declare sub OS_WaitBSemaphoreTask(bylabel task_label as word , byval hBSem as byte)       ' переводит указанную задачу в состояние "ждать освобождения семафора hBSem", если он "занят" (АПУД), иначе продолжать
declare sub OS_BusyBSemaphore(byval hBSem as byte)                              ' устанавливает семафор hBSem в состояние "занято"
declare sub OS_FreeBSemaphore(byval hBSem as byte)                              ' устанавливает семафор hBSem в состояние "свободно"
' сервис счетных семафоров
'declare function OS_CreateCSemaphore() as byte                                  ' возвращает уникальный идентификатор счетного семафора hСSem
'declare sub OS_WaitCSemaphore(byval hCSem as byte , byval bSemValue as byte)    ' переводит текущую задачу в состояние "ждать значение bSemValue семафора hСSem", (АПУД)
'declare sub OS_WaitCSemaphoreTask(bylabel task_label as word , byval hCSem as byte , byval bSemValue as byte)       ' переводит задачу task_label в состояние "ждать значение bSemValue семафора hСSem", (АПУД)
'declare sub OS_IncCSemaphore(byval hCSem as byte)                               ' увеличивает счетный семафор hCSem на 1
'declare sub OS_DecCSemaphore(byval hCSem as byte)                               ' уменьшает счетный семафор hCSem на 1
'declare sub OS_SetCSemaphore(byval hCSem as byte , byval bSemValue as byte)     ' устанавливает семафору hCSem значение bSemValue

' --- управление задачами ---
sub OS_InitTask(word ptr task_label , byte task_prio)                           ' заносит в таблицу task_adr_lo/hi(i) адрес старта задачи, в task_prior(i)<-prior, task_delay(i)<-0
   incr task_count
   if task_count > OS_MAX_TASK then
      OS_RaiseErr OSERR_TASK_MAX_REACHED
      decr task_count
      exit sub
   end if
   'disable timer0
   task_ptr = task_label                                                        ' получить адрес зачади 1
   task_entry(task_count) = task_ptr                                            ' сохраним точку входа (как адрес метки, т.е. удвоенный) задачи, чтобы потом быстро искать ее ID
   shift task_ptr , right                                                       ' поделить адрес надвое, чтобы получить реальное значение РС
   task_adr_lo(task_count) = task_ptr_lo                                        ' сохраняем адрес входа как текущий указатель исполнения задачи
   task_adr_hi(task_count) = task_ptr_hi
   task_state(task_count) = OSTS_UNDEFINE
   task_prior(task_count) = task_prio
   task_delay(task_count) = 0
   task_event(task_count) = 0
   debug "ADDED TASK ID=" ; task_count ; " on adr 0x" ; hex(task_ptr)
   'enable timer0
end sub
sub OS_Stop()                                                                   ' ставит текущей задаче статус "остановлена" (OSTS_STOP); дальнейший рестарт возможен только с начальной точки входа задачи , а не места останова
   disable timer0
   task_ptr = task_entry(task_current)                                          ' возвращаем остановленной задаче адрес ее старта
   shift task_ptr , right                                                       ' поделить адрес надвое
   task_adr_lo(task_current) = task_ptr_lo
   task_adr_hi(task_current) = task_ptr_hi
   task_delay(task_current) = 0
   task_event(task_count) = 0
   task_state(task_current) = OSTS_STOP
   'enable timer0
   goto shedule
end sub
sub OS_StopTask(task_label as word)                                             ' ставит указанной задаче статус "остановлена" (OSTS_STOP) ; дальнейший рестарт возможен только с начальной точки входа задачи, а не места останова
   disable timer0
   MGetTaskIdByLabel
   task_ptr = task_entry(task_id)                                               ' возвращаем остановленной задаче адрес ее старта
   shift task_ptr , right                                                       ' поделить адрес надвое
   task_adr_lo(task_id) = task_ptr_lo
   task_adr_hi(task_id) = task_ptr_hi
   task_state(task_id) = OSTS_STOP
   goto shedule
end sub
sub OS_Pause()                                                                  ' ставит текущей задаче статус "остановлена" (OSTS_STOP); дальнейший рестарт с места останова
   task_state(task_current) = OSTS_PAUSE
   goto shedule
end sub
sub OS_PauseTask(task_label as word)                                            ' ставит указанной задаче статус "остановлена" (OSTS_STOP) ; дальнейший рестарт возможен только с начальной точки входа задачи, а не места останова
   MGetTaskIdByLabel
   task_state(task_id) = OSTS_PAUSE
   goto shedule
end sub
sub OS_Resume()                                                                 ' ставит задаче статус "на исполнение"  (OSTS_READY)
   task_state(task_current) = OSTS_READY
   'goto shedule
end sub
sub OS_ResumeTask(task_label as word)                                           ' ставит задаче статус "на исполнение"  (OSTS_READY)
   'disable timer0
   MGetTaskIdByLabel
   task_state(task_id) = OSTS_READY
end sub
sub OS_Restart()
   disable timer0
   task_ptr = task_entry(task_current)                                          ' возвращаем остановленной задаче адрес ее старта
   shift task_ptr , right                                                       ' поделить адрес надвое
   task_adr_lo(task_current) = task_ptr_lo
   task_adr_hi(task_current) = task_ptr_hi
   task_delay(task_current) = 0
   task_event(task_count) = 0
   task_state(task_id) = OSTS_RESTART
   enable timer0
   goto shedule
end sub

' --- сервис таймеров ---
sub OS_Delay(word delay_time)                                                   ' ставит задаче статус "отложена на время" (OSTS_DELAY)
   disable timer0
   task_delay(task_current) = delay_time
   task_state(task_current) = OSTS_DELAY
   goto shedule
end sub
sub OS_DelayTask(word ptr task_label , word delay_time)                         ' ставит задаче статус "отложена на время" (OSTS_DELAY)
   disable timer0
   MGetTaskIdByLabel
   enable timer0
   task_delay(task_id) = delay_time
   task_state(task_id) = OSTS_DELAY
   goto shedule
end sub

' --- сервис событий ---
function OS_CreateEvent()                                                       ' возвращает уникальный дексриптор hEvent, ссылающийся на пользовательское событие
   incr event_count
   OS_CreateEvent = event_count
end function
sub OS_WaitEvent(byte hEvent)                                                   ' заставляет задачу ждать событие hEvent;
   task_event(task_current) = hEvent                                            ' устанавливает задаче статус WAIT
   task_state(task_current) = OSTS_WAIT                                         ' и передает управление диспетчеру
   goto shedule
end sub
sub OS_WaitEventTask(word ptr task_label , byte hEvent)
   MGetTaskIdByLabel
   task_event(task_id) = hEvent                                                 ' устанавливает задаче статус WAIT
   task_state(task_id) = OSTS_WAIT                                              ' и передает управление диспетчеру
   goto shedule
end sub
sub OS_WaitEventTO(hEvent as byte , wTimeout as word)
   disable timer0
   reset OS_TIMEOUT
   task_event(task_current) = hEvent                                            ' устанавливает задаче статус WAIT
   task_delay(task_current) = wTimeout                                          ' установим задержку
   task_state(task_current) = OSTS_WAIT                                         ' и передает управление диспетчеру
   goto shedule
end sub
sub OS_SignalEvent(byte hEvent)                                                 ' когда в приложении происходит событие hEvent, приложение вызывает эту функцию
    for task_id = 1 to task_count                                               ' посмотрим все задачи
      if task_state(task_id) = OSTS_WAIT then                                   ' имеет ли задача статус WAIT?
         if task_event(task_id) = hEvent then                                   ' ждет ли такая задача событие hEvent?
            task_event(task_id) = NULL                                          ' ****** 1.05 сбросим тему этой задаче
            task_delay(task_id) = NULL                                          ' ****** 1.05 и таймаут
            task_state(task_id) = OSTS_READY                                    ' установить такой задаче статус READY
         end if
      end if
   next
 end sub

 ' --- сервис сообщений ---
function OS_CreateMessage()                                                     ' возвращает уникальный дескриптор темы hTopic, ссылающейся на пользовательские сообщения по этой теме
   incr topic_count
   if topic_count > OS_MAX_TOPIC then
      OS_RaiseErr OSERR_TOPIC_MAX_REACHED                                       ' если ошибка (достигнут предел числа тем), вызаывает пользовательский обработчик
      decr task_count
      OS_CreateMessage = NULL                                                   ' и возращает 0
   else
      OS_CreateMessage = topic_count
   end if
end function
sub OS_WaitMessage(byte hTopic)                                                 ' заставляет задачу ждать сообщение c темой hTopic; сообщение может быть прочитано через OS_GetMessage
   task_topic(task_current) = hTopic                                            ' устанавливает задаче статус WAIT
   task_state(task_current) = OSTS_WAIT                                         ' и передает управление диспетчеру
   goto shedule
end sub
sub OS_WaitMessageTO(byte hTopic , word os_timeout)                             ' заставляет задачу ждать сообщение c темой hTopic; сообщение может быть прочитано через OS_GetMessage
   reset OS_TIMEOUT
   task_delay(task_current) = wTimeout                                          ' установим задержку
   task_topic(task_current) = hTopic                                            ' устанавливает задаче статус WAIT
   task_state(task_current) = OSTS_WAIT                                         ' и передает управление диспетчеру
   goto shedule
end sub
sub OS_WaitMessageTask(word ptr task_label , byte hTopic)                       ' заставляет задачу ждать сообщение c темой hTopic; сообщение может быть прочитано через OS_GetMessage
   MGetTaskIdByLabel
   task_topic(task_id) = hTopic                                                 ' устанавливает задаче статус WAIT
   task_state(task_id) = OSTS_WAIT                                              ' и передает управление диспетчеру
   goto shedule
end sub
sub OS_SendMessage(byte hTopic , word wMessage)                                 ' когда приложению нужно послать сообщение с темой hTopic, приложение вызывает эту функцию
    for task_id = 1 to task_count                                               ' посмотрим все задачи
      if task_state(task_id) = OSTS_WAIT then                                   ' имеет ли задача статус WAIT?
         if task_topic(task_id) = hTopic then                                   ' ждет ли такая задача сообщений на тему hTopic?
            os_message(hTopic) = wMessage                                       ' сохраним сообщение
            task_state(task_id) = OSTS_READY                                    ' установить такой задаче статус READY
         end if
      end if
   next
 end sub
function OS_GetMessage(byte hTopic) word                                        ' возвращает word сообщение по теме hTopic, после чего очищает его
   OS_GetMessage = os_message(hTopic)
   os_message(hTopic) = NULL
end function
function OS_PeekMessage(byte hTopic)                                            ' возвращает word сообщение по теме hTopic, но не очищает его
   OS_PeekMessage = os_message(hTopic)
end function
function OS_GetMessageString(byte hTopic) as string                             ' возвращает строку сообщения по теме hTopic, после чего очищает его
   OS_GetMessageString = ""
   os_tmpword = os_message(hTopic)
   do
      os_tmpbyte = inp(os_tmpword)
      OS_GetMessageString = OS_GetMessageString + chr(os_tmpbyte)
      incr os_tmpword
   loop until os_tmpbyte = 0
   out os_message(hTopic) , 0                                                   ' clear message (????? erase user string!!!????)
   os_message(hTopic) = NULL                                                    ' clear address
end function
function OS_PeekMessageString(byte hTopic) as string                            ' возвращает строку сообщения по теме hTopic, но не очищает его
   OS_PeekMessageString = ""
   os_tmpword = os_message(hTopic)
   do
      os_tmpbyte = inp(os_tmpword)
      OS_PeekMessageString = OS_PeekMessageString + chr(os_tmpbyte)
      incr os_tmpword
   loop until os_tmpbyte = 0
end function


' --- сервис бинарных семафоров ---
function OS_CreateBSemaphore() as byte                                          ' возвращает уникальный идентификатор бинарного семафора hBSem
   incr bSem_count
   if bSem_count > 16 then
      OS_RaiseErr OSERR_BSEM_MAX_REACHED                                        ' если ошибка (достигнут предел числа тем), вызаывает пользовательский обработчик
   end if
   OS_CreateBSemaphore = bSem_count - 1
end function
sub OS_WaitBSemaphore(byval hBSem as byte)                                      ' устанавливает текущей задаче состояние "ждать освобождения семафора hBSem", если занят (АПУД), иначе продолжать
   if bSem_reg.hBSem = TRUE then                                                ' проверка, занят ли семафор hBSem?
      set task_bsem(task_current).hBSem                                         ' устанавливает в task_bsem(текущая задача) бит номер hBsem
      task_state(task_current) = OSTS_WAIT                                      ' устанавливает текущей задаче статус WAIT
      goto shedule                                                              ' и передает управление диспетчеру
   end if
end sub
sub OS_WaitBSemaphoreTask(bylabel task_label as word , byval hBSem as byte)     ' устанавливает заданной задаче состояние "ждать" освобождения семафора hBSem, (АПУД)
   if bSem_reg.hBSem = TRUE then                                                ' проверка, занят ли семафор hBSem?
      MGetTaskIdByLabel
      set task_bsem(task_id).hBSem                                              ' устанавливает в task_bsem(&task_label) бит номер hBsem
      task_state(task_id) = OSTS_WAIT                                           ' устанавливает задаче &task_label статус WAIT
      goto shedule                                                              ' и передает управление диспетчеру
   end if
end sub
sub OS_BusyBSemaphore(byval hBSem as byte)                                      ' устанавливает семафор hBSem в состояние "занято"
   set bSem_reg.hBSem
end sub
sub OS_FreeBSemaphore(byval hBSem as byte)                                      ' устанавливает семафор hBSem в состояние "свободно"
    for task_id = 1 to task_count                                               ' посмотрим все задачи:
      if task_state(task_id) = OSTS_WAIT then                                   ' имеет ли задача статус WAIT?
         if task_bsem(task_id).hBsem = TRUE and bSem_reg.hBSem = TRUE then      ' ждет ли такая задача освобождения семафора hBSem?
            task_bsem(task_id).hBsem = FALSE                                    ' сброс ожидания семафора у задачи
            bSem_reg.hBSem = FALSE                                              ' сброс самого семафора
            task_state(task_id) = OSTS_READY                                    ' установить задаче статус READY
         end if
      end if
   next
end sub

' --- сервис счетных семафоров ---
'function OS_CreateCSemaphore() as byte                                          ' возвращает уникальный идентификатор счетного семафора hСSem
'end function
'sub OS_WaitCSemaphore(byval hCSem as byte , byval bSemValue as byte)            ' устанавливает текущей задаче состояние "ждать"  значение bSemValue семафора hСSem, (АПУД)
'end sub
'sub OS_WaitCSemaphoreTask(bylabel task_label as word , byval hCSem as byte , byval bSemValue as byte)       ' устанавливает заданной задаче состояние "ждать" значение bSemValue семафора hСSem, (АПУД)
'end sub
'sub OS_IncCSemaphore(byval hCSem as byte)                                       ' увеличивает счетный семафор hCSem на 1
'end sub
'sub OS_DecCSemaphore(byval hCSem as byte)                                       ' уменьшает счетный семафор hCSem на 1
'end sub
'sub OS_SetCSemaphore(byval hCSem as byte , byval bSemValue as byte)             ' устанавливает семафору hCSem значение bSemValue
'end sub

' --- системный сервис ---
sub OS_Init(word error_trap)
   user_err_trap = error_trap                                                   ' сохраним адрес пользовательского обработчика ошибок
   shift user_err_trap , right
   #if OS_SIM = TRUE
      config TIMER0 = counter , prescale = 1                                    ' настройка таймера. для симуляции prescale=1
   #else
      config TIMER0 = counter , prescale = 8                                    ' настройка таймера. для реала prescale=8
   #endif
   on timer0 tim0_isr save
   for task_id = 1 to OS_MAX_TASK
      task_queue(task_id) = task_id
   next
   task_current = NULL                                                          ' никакая задача не запущена
   task_count = NULL                                                            ' число инициализированных задач нуль
   enable interrupts
   disable timer0                                                               ' запускаем таймерную службу
end sub
sub OS_RaiseErr(byte os_err_num)
   if os_err_num <> 0 then
      !STS {RR16},R16
      !LDS R16, {user_err_trap_lo}                                              ' загрузим в стек адрес задачи
      !PUSH R16
      !LDS R16, {user_err_trap_hi}
      !PUSH R16
      !LDS R16,{RR16}
      return
      ' на самом деле будет вызван пользовательский обработчик
      ' параметр был помещен в софтовый стек по адресу у+0, у+1, и будет извлечен обработчиком,
      ' а хардовый и софтовый стеки вернутся в начальное состояние по выходу из обработчика
   end if
end sub
sub OS_Sheduler()
   nop
   shedule:
      disable timer0
      disable interrupts
      nop
   ' останавливаем запущенную задачу,если была запушена
      if task_current <> 0 then
         if task_state(task_current) = OSTS_RUN then                            ' если задача не имеет "отложенных" статусов WAIT, DELAY и т.п.
            task_state(task_current) = OSTS_READY                               ' то ставим ей статус -x-STOP-x-  READY (1.05!)
         end if
         if task_state(task_current) <> OSTS_STOP or task_state(task_current) <> OSTS_RESTART then       ' задача со статусом стоп будет стартовать с начала
            ' сохраняем контекст задачи: вытащим из стека адрес возврата к тому месту, где произошел переход в шедулер
            !STS {RR16},R16                                                     ' сохраним R16 в памяти
            !pop R16
            !sts {task_ptr_hi},R16
            !pop R16
            !sts {task_ptr_lo},R16
            !LDS R16,{RR16}                                                     ' восстановим R16
            ' сохраним адрес возврата к точке перехода задачи в шедулер
            task_adr_lo(task_current) = task_ptr_lo
            task_adr_hi(task_current) = task_ptr_hi
         end if
         if task_state(task_current) = OSTS_RESTART then task_state(task_current) = OSTS_READY
         task_current = 0                                                       ' никакая задача не запущена
      end if

   ' *** работаем с очередью задач ***
     ' ищем задачу с наивысшим (минимальное число) приоритетом и статусом OSTS_READY
      minprio = 255
      for task_id = 1 to OS_MAX_TASK
         debug "in queue(" ; task_id ; ")=" ; task_queue(task_id)
         if task_state(task_queue(task_id)) <> OSTS_READY then continue
         if task_prior(task_queue(task_id)) < minprio then
            minprio = task_prior(task_queue(task_id))
            minqueuidx = task_id                                                ' указывает на индекс очереди
         end if
      next

      if minprio > OS_MAX_TASK then
      ' нет ни одной задачи со статусом READY
         enable timer0
         enable interrupts
         nop                                                                    ' чтобы успело отработать прерывание таймера
         goto shedule                                                           ' зацикливаем шедулинг, если нет задач
      end if

   ' вытащим из очереди приоритетное задание, подвинем очередь, и поставим его в конец
      task_current = task_queue(minqueuidx)
      incr minqueuidx
      for task_id = minqueuidx to OS_MAX_TASK
         task_queue(task_id - 1) = task_queue(task_id)
      next
      task_queue(OS_MAX_TASK) = task_current

      for task_id = 1 to OS_MAX_TASK
         debug "out queue(" ; task_id ; ")=" ; task_queue(task_id)
      next

      debug "Run " ; task_current ; " at 0x" ; hex(task_adr_hi(task_current)) ; hex(task_adr_lo(task_current))

   ' запустим приоритетную задачу
      task_state(task_current) = OSTS_RUN
      task_ptr_lo = task_adr_lo(task_current)
      task_ptr_hi = task_adr_hi(task_current)
      !STS {RR16},R16                                                           ' сохраним R16 в памяти
      !LDS R16,{task_ptr_lo}                                                    ' загрузим в стек адрес задачи
      !PUSH R16
      !LDS R16,{task_ptr_hi}
      !PUSH R16
      !LDS R16,{RR16}                                                           ' восстановим R16
      enable interrupts
      enable timer0                                                             ' не забудем включить таймер
   return                                                                       ' на самом деле переход к задаче
end sub

' *********************************************************************************************************************
' --- обработчик прерывания таймера ---
tim0_isr:
   incr tim_cnt
   #if OS_SIM = TRUE
      if tim_cnt = 1 then
   #else
      if tim_cnt = 7 then
   #endif
      tim_cnt = 0
      'toggle portb.5
      for isr_i = 1 to task_count
         if task_delay(isr_i) > 0 then
            decr task_delay(isr_i)                                              ' декрементируем задержку у отложенных задач
            if task_delay(isr_i) = 0 then                                       ' истекла ли задержка
               if task_state(isr_i) = OSTS_WAIT then set OS_TIMEOUT
               task_state(isr_i) = OSTS_READY                                   ' ставим задаче статус READY
            end if
         end if
      next
   end if
   set TIFR0.TOV0
return


RTOS_done: