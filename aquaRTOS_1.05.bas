' ********************************************************************************************
'                                   A-RTOS 1.05
'
'                             by Alexandra Bernshtein
'                               vk.com/id260853760
'                                 fausto@mail.ru
' ********************************************************************************************

' ������� �� ������ 1.04
' ��������� ����������� ���������� � �������� ��������� (������ Messages) ��������� �� ������
'
$nocompile

#if varexist( "NULL")
#else
   const NULL = 0                                                               ' �����
#endif
#if varexist( "FALSE")
#else
   const FALSE = NULL
#endif
#if varexist( "TRUE")
#else
   const TRUE = 1
#endif

' --- ������������� ������������� ��������� -----

const OS_SIM = TRUE
                                                         ' �� �������� ������ / �� ����������

debug off

#if OS_SIM = TRUE
   $sim
#endif

const OS_MAX_TASK = 8                                                           ' ������������ ����� ����� � �������. ������ ������ ������� __12__ ���� ������
const OS_MAX_TOPIC = 16                                                         ' ������������ ����� ��� ���������
'const OS_MAX_CSEM = 8                                                           ' ������������ ����� ������� ���������

' --- ����� ������������� ������������� ���������� -----

const OSTS_UNDEFINE = NULL                                                      ' ������� ������
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
const OSERR_TIMEOUT = 5                                                         ' ����� ������������� �������
const OSERR_CANT_FIND_TASK = 64



dim task_ptr as word                                                            ' ��������� ���������� ��� ��������� ����� ����� � ������
dim task_ptr_lo as byte at task_ptr overlay
dim task_ptr_hi as byte at task_ptr + 1 overlay

dim task_entry(OS_MAX_TASK) as word                                             ' ����� ����� ������; ������ ������, �� ������� ����� ��������� �� ������; �� ������� �� ���� ���������� ID ������
dim task_adr_lo(OS_MAX_TASK) as byte                                            ' ������� ���� �������� ������ ���������� ������
dim task_adr_hi(OS_MAX_TASK) as byte                                            ' �������
dim task_state(OS_MAX_TASK) as byte                                             ' ������ ������ OSTS_*
dim task_prior(OS_MAX_TASK) as byte                                             ' ��������� ������, 0 - ������, 15 - ������
dim task_queue(OS_MAX_TASK) as byte                                             ' ������� �����
dim task_delay(OS_MAX_TASK) as word                                             ' ��������, � ����� ���������� �������
dim task_event(OS_MAX_TASK) as byte                                             ' �������, �� ������� ��������� ������
dim task_topic(OS_MAX_TASK) as byte                                             ' ����, ��������� �� ������� ���� ������
dim task_bsem(OS_MAX_TASK) as word                                              ' ������� ��������, ������������ ������� ���� ������ (������ ��� ����� �����-�� = ������� ����� �����-��)

dim task_count as byte                                                          ' ������� ����� ������������ � ������� �����
dim task_current as byte                                                        ' id ����������� ������

dim user_err_trap as word                                                       ' ����� ����������������� ����������� ������
dim user_err_trap_lo as byte at user_err_trap overlay
dim user_err_trap_hi as byte at user_err_trap + 1 overlay

dim minprio as byte
dim minqueuidx as byte
dim task_id as byte                                                             ' id ������
dim tim_cnt as word                                                             ' ������� ���������� �������
dim RR16 as byte

dim OS_TIMEOUT as boolean

dim event_count as byte                                                         ' ������� �������, ������������� ���������� ����� ������� ������ �������
dim topic_count as byte                                                         ' ������� ��� ���������, ������������� ���������� ����� ���� ������ ���������
dim os_message(OS_MAX_TOPIC) as word                                            ' ��������� (!word � 105!)
dim bSem_count as byte                                                          ' ������� �������� ���������
dim bSem_reg as word                                                            ' ������� �������� ���������: ������ ��� ����� �����-�� = ������� ����� �����-��
'dim CntSemaphore(OS_MAX_CSEM) as byte                                           ' ������� ��������
'dim CntSem_count as byte                                                        ' ������� ������� ���������

dim os_tmpbyte as byte
dim os_tmpword as word

const DELAY_NONE = NULL

dim isr_i as byte                                                               ' index in interrupt

disable timer0

goto RTOS_done                                                                  ' required by BASCOM compilier if used "config  submode = old"

macro MGetTaskIdByLabel
   for task_id = 1 to task_count                                                ' �� ������ ������ � task_label ������� �� task_id � ������� ��� ���������� task_id := 0
      if task_entry(task_id) = task_label then exit for
   next
   if task_id > task_count then
      OS_RaiseErr OSERR_CANT_FIND_TASK or OSERR_CRITICAL
   end if
end macro

' --- ��������� ������� ---
' ��������� ������
declare sub OS_Init(bylabel error_trap as word)                                 ' ������ ���� ������� ����� �������� RTOS ; ���������������� ���������� ������ ������ ���� ���������� � ����� ���������� ���� byte, ��������, MyErrTrap(err_num as byte).
declare sub OS_Sheduler()                                                       ' �������� ���������; ������ ����������� �������� ���������� �� ������ � ����������; ������, ���������� ���� ������, (!!�� - 105!!) ���������������, ��������� ���������� ���������� (� ������ �������, ���� ����)
declare sub OS_RaiseErr(byval os_err_num as byte)                               ' �������� ���������������� ���������� ������
' ���������� ��������
declare sub OS_InitTask(bylabel task_label as word , byval task_prio as byte)   ' ����� ������, ������ ��� ���� ������������, ������ ���� ���������������� � �������. ���������� ��� �����.
declare sub OS_Stop()                                                           ' ���������� ������� ������ (����). ���� � ���������� ��� ������ ��������� �� ����������, ��� �������� � ������, � �� � ����� ��������
declare sub OS_StopTask(bylabel task_label as word)                             ' ���������� ��������� ������ (����). ���� � ���������� ��� ������ ��������� �� ����������, ��� �������� � ������, � �� � ����� ��������
declare sub OS_Pause()                                                          ' 105: ������������� ������� ������ (����). ���� � ���������� ��� ������ ��������� �� ����������, ��� �������� � ����� �����������
declare sub OS_PauseTask(bylabel task_label as word)                            ' 105: ������������� ��������� ������ (����). ���� � ���������� ��� ������ ��������� �� ����������, ��� �������� � ����� �����������
declare sub OS_Resume()                                                         ' ��������� ��������� ������ ������ "������ � ����������", !!! �� �������� ���������� ���������� ������������� !!!  ; ��� ����� ������� ��� ������ OS_Sheduler
declare sub OS_ResumeTask(bylabel task_label as word)                           ' ��������� ��������� ������ ������ "������ � ����������", !!! �� �������� ���������� ���������� ������������� !!!  ; ��� ����� ������� ��� ������ OS_Sheduler
declare sub OS_Restart()                                                        ' ������������� ������� ������ � ������ (����)

' ������ ��������
declare sub OS_Delay(byval delay_time as word)                                  ' ��������� ������� ������ � ��������� "�������� �� delay_time", �������������� �������� ���������� ���������� (����)
declare sub OS_DelayTask(bylabel task_label as word , byval delay_time as word) ' ��������� ��������� ������ � ��������� "�������� �� delay_time", (����).
' ������ �������
declare function OS_CreateEvent() as byte                                       ' ���������� ���������� ������������� ������� hEvent
declare sub OS_WaitEvent(byval hEvent as byte)                                  ' ��������� ������� ������ � ��������� "����� ������� hEvent", (����)
declare sub OS_WaitEventTask(bylabel task_label as word , byval hEvent as byte) ' ��������� ��������� ������ ��������� "����� ������� hEvent", (����)
declare sub OS_WaitEventTO(byval hEvent as byte , byval wTimeout as word)       ' ��������� ������� ������ � ��������� "����� ������� hEvent" � ���������, ���������� ���� OS_Timeout (����)
declare sub OS_SignalEvent(byval hEvent as byte)                                ' ������������� ���������� ������, ����� ������������ � ������� � ������� hEvent
' ������ ���������
declare function OS_CreateMessage() as byte                                     ' ��������� ���������� ������������� ���� hTopic, �� ������� ����� ������������ ���������
declare sub OS_WaitMessage(byval hTopic as byte)                                ' ��������� ������� ������ � ��������� "����� ��������� �� ���� hTopic", (����)
declare sub OS_WaitMessageTask(bylabel task_label as word , byval hTopic as byte)       ' ��������� ��������� ������ task_label � ��������� "����� ��������� �� ���� hTopic", (����)     (!word in 105!)
declare sub OS_WaitMessageTO(byval hTopic as byte , byval wTimeout as word)     ' ��������� ������� ������ � ��������� "����� ��������� �� ���� hTopic", (����)
declare sub OS_SendMessage(byval hTopic as byte , byval wMessage as word)       ' ������������� ���������� ������ �������� ��������� wMessage �� ���� hTopic; wMessage ����� ��������� �� ����� ����������-���������
declare function OS_GetMessage(byval hTopic as byte) as word                    ' ������ ��������� ��������� � ���������� ���        (!word in 105!)
declare function OS_PeekMessage(byval hTopic as byte) as word                   ' ������ ��������� ��������� � �� ���������� ���        (!word in 105!)
declare function OS_GetMessageString(byval hTopic as byte) as string            ' ���������� ������ ��������� �� ���� hTopic, ����� ���� ������� ���
declare function OS_PeekMessageString(byval hTopic as byte) as string           ' ���������� ������ ��������� �� ���� hTopic, �� �� ������� ���
' ������ �������� ���������
declare function OS_CreateBSemaphore() as byte                                  ' ���������� ���������� ������������� ��������� �������� hBSem
declare sub OS_WaitBSemaphore(byval hBSem as byte)                              ' ��������� ������� ������ � ��������� "����� ������������ �������� hBSem", ���� �� "�����" (����), ����� ����������
declare sub OS_WaitBSemaphoreTask(bylabel task_label as word , byval hBSem as byte)       ' ��������� ��������� ������ � ��������� "����� ������������ �������� hBSem", ���� �� "�����" (����), ����� ����������
declare sub OS_BusyBSemaphore(byval hBSem as byte)                              ' ������������� ������� hBSem � ��������� "������"
declare sub OS_FreeBSemaphore(byval hBSem as byte)                              ' ������������� ������� hBSem � ��������� "��������"
' ������ ������� ���������
'declare function OS_CreateCSemaphore() as byte                                  ' ���������� ���������� ������������� �������� �������� h�Sem
'declare sub OS_WaitCSemaphore(byval hCSem as byte , byval bSemValue as byte)    ' ��������� ������� ������ � ��������� "����� �������� bSemValue �������� h�Sem", (����)
'declare sub OS_WaitCSemaphoreTask(bylabel task_label as word , byval hCSem as byte , byval bSemValue as byte)       ' ��������� ������ task_label � ��������� "����� �������� bSemValue �������� h�Sem", (����)
'declare sub OS_IncCSemaphore(byval hCSem as byte)                               ' ����������� ������� ������� hCSem �� 1
'declare sub OS_DecCSemaphore(byval hCSem as byte)                               ' ��������� ������� ������� hCSem �� 1
'declare sub OS_SetCSemaphore(byval hCSem as byte , byval bSemValue as byte)     ' ������������� �������� hCSem �������� bSemValue

' --- ���������� �������� ---
sub OS_InitTask(word ptr task_label , byte task_prio)                           ' ������� � ������� task_adr_lo/hi(i) ����� ������ ������, � task_prior(i)<-prior, task_delay(i)<-0
   incr task_count
   if task_count > OS_MAX_TASK then
      OS_RaiseErr OSERR_TASK_MAX_REACHED
      decr task_count
      exit sub
   end if
   'disable timer0
   task_ptr = task_label                                                        ' �������� ����� ������ 1
   task_entry(task_count) = task_ptr                                            ' �������� ����� ����� (��� ����� �����, �.�. ���������) ������, ����� ����� ������ ������ �� ID
   shift task_ptr , right                                                       ' �������� ����� ������, ����� �������� �������� �������� ��
   task_adr_lo(task_count) = task_ptr_lo                                        ' ��������� ����� ����� ��� ������� ��������� ���������� ������
   task_adr_hi(task_count) = task_ptr_hi
   task_state(task_count) = OSTS_UNDEFINE
   task_prior(task_count) = task_prio
   task_delay(task_count) = 0
   task_event(task_count) = 0
   debug "ADDED TASK ID=" ; task_count ; " on adr 0x" ; hex(task_ptr)
   'enable timer0
end sub
sub OS_Stop()                                                                   ' ������ ������� ������ ������ "�����������" (OSTS_STOP); ���������� ������� �������� ������ � ��������� ����� ����� ������ , � �� ����� ��������
   disable timer0
   task_ptr = task_entry(task_current)                                          ' ���������� ������������� ������ ����� �� ������
   shift task_ptr , right                                                       ' �������� ����� ������
   task_adr_lo(task_current) = task_ptr_lo
   task_adr_hi(task_current) = task_ptr_hi
   task_delay(task_current) = 0
   task_event(task_count) = 0
   task_state(task_current) = OSTS_STOP
   'enable timer0
   goto shedule
end sub
sub OS_StopTask(task_label as word)                                             ' ������ ��������� ������ ������ "�����������" (OSTS_STOP) ; ���������� ������� �������� ������ � ��������� ����� ����� ������, � �� ����� ��������
   disable timer0
   MGetTaskIdByLabel
   task_ptr = task_entry(task_id)                                               ' ���������� ������������� ������ ����� �� ������
   shift task_ptr , right                                                       ' �������� ����� ������
   task_adr_lo(task_id) = task_ptr_lo
   task_adr_hi(task_id) = task_ptr_hi
   task_state(task_id) = OSTS_STOP
   goto shedule
end sub
sub OS_Pause()                                                                  ' ������ ������� ������ ������ "�����������" (OSTS_STOP); ���������� ������� � ����� ��������
   task_state(task_current) = OSTS_PAUSE
   goto shedule
end sub
sub OS_PauseTask(task_label as word)                                            ' ������ ��������� ������ ������ "�����������" (OSTS_STOP) ; ���������� ������� �������� ������ � ��������� ����� ����� ������, � �� ����� ��������
   MGetTaskIdByLabel
   task_state(task_id) = OSTS_PAUSE
   goto shedule
end sub
sub OS_Resume()                                                                 ' ������ ������ ������ "�� ����������"  (OSTS_READY)
   task_state(task_current) = OSTS_READY
   'goto shedule
end sub
sub OS_ResumeTask(task_label as word)                                           ' ������ ������ ������ "�� ����������"  (OSTS_READY)
   'disable timer0
   MGetTaskIdByLabel
   task_state(task_id) = OSTS_READY
end sub
sub OS_Restart()
   disable timer0
   task_ptr = task_entry(task_current)                                          ' ���������� ������������� ������ ����� �� ������
   shift task_ptr , right                                                       ' �������� ����� ������
   task_adr_lo(task_current) = task_ptr_lo
   task_adr_hi(task_current) = task_ptr_hi
   task_delay(task_current) = 0
   task_event(task_count) = 0
   task_state(task_id) = OSTS_RESTART
   enable timer0
   goto shedule
end sub

' --- ������ �������� ---
sub OS_Delay(word delay_time)                                                   ' ������ ������ ������ "�������� �� �����" (OSTS_DELAY)
   disable timer0
   task_delay(task_current) = delay_time
   task_state(task_current) = OSTS_DELAY
   goto shedule
end sub
sub OS_DelayTask(word ptr task_label , word delay_time)                         ' ������ ������ ������ "�������� �� �����" (OSTS_DELAY)
   disable timer0
   MGetTaskIdByLabel
   enable timer0
   task_delay(task_id) = delay_time
   task_state(task_id) = OSTS_DELAY
   goto shedule
end sub

' --- ������ ������� ---
function OS_CreateEvent()                                                       ' ���������� ���������� ���������� hEvent, ����������� �� ���������������� �������
   incr event_count
   OS_CreateEvent = event_count
end function
sub OS_WaitEvent(byte hEvent)                                                   ' ���������� ������ ����� ������� hEvent;
   task_event(task_current) = hEvent                                            ' ������������� ������ ������ WAIT
   task_state(task_current) = OSTS_WAIT                                         ' � �������� ���������� ����������
   goto shedule
end sub
sub OS_WaitEventTask(word ptr task_label , byte hEvent)
   MGetTaskIdByLabel
   task_event(task_id) = hEvent                                                 ' ������������� ������ ������ WAIT
   task_state(task_id) = OSTS_WAIT                                              ' � �������� ���������� ����������
   goto shedule
end sub
sub OS_WaitEventTO(hEvent as byte , wTimeout as word)
   disable timer0
   reset OS_TIMEOUT
   task_event(task_current) = hEvent                                            ' ������������� ������ ������ WAIT
   task_delay(task_current) = wTimeout                                          ' ��������� ��������
   task_state(task_current) = OSTS_WAIT                                         ' � �������� ���������� ����������
   goto shedule
end sub
sub OS_SignalEvent(byte hEvent)                                                 ' ����� � ���������� ���������� ������� hEvent, ���������� �������� ��� �������
    for task_id = 1 to task_count                                               ' ��������� ��� ������
      if task_state(task_id) = OSTS_WAIT then                                   ' ����� �� ������ ������ WAIT?
         if task_event(task_id) = hEvent then                                   ' ���� �� ����� ������ ������� hEvent?
            task_event(task_id) = NULL                                          ' ****** 1.05 ������� ���� ���� ������
            task_delay(task_id) = NULL                                          ' ****** 1.05 � �������
            task_state(task_id) = OSTS_READY                                    ' ���������� ����� ������ ������ READY
         end if
      end if
   next
 end sub

 ' --- ������ ��������� ---
function OS_CreateMessage()                                                     ' ���������� ���������� ���������� ���� hTopic, ����������� �� ���������������� ��������� �� ���� ����
   incr topic_count
   if topic_count > OS_MAX_TOPIC then
      OS_RaiseErr OSERR_TOPIC_MAX_REACHED                                       ' ���� ������ (��������� ������ ����� ���), ��������� ���������������� ����������
      decr task_count
      OS_CreateMessage = NULL                                                   ' � ��������� 0
   else
      OS_CreateMessage = topic_count
   end if
end function
sub OS_WaitMessage(byte hTopic)                                                 ' ���������� ������ ����� ��������� c ����� hTopic; ��������� ����� ���� ��������� ����� OS_GetMessage
   task_topic(task_current) = hTopic                                            ' ������������� ������ ������ WAIT
   task_state(task_current) = OSTS_WAIT                                         ' � �������� ���������� ����������
   goto shedule
end sub
sub OS_WaitMessageTO(byte hTopic , word os_timeout)                             ' ���������� ������ ����� ��������� c ����� hTopic; ��������� ����� ���� ��������� ����� OS_GetMessage
   reset OS_TIMEOUT
   task_delay(task_current) = wTimeout                                          ' ��������� ��������
   task_topic(task_current) = hTopic                                            ' ������������� ������ ������ WAIT
   task_state(task_current) = OSTS_WAIT                                         ' � �������� ���������� ����������
   goto shedule
end sub
sub OS_WaitMessageTask(word ptr task_label , byte hTopic)                       ' ���������� ������ ����� ��������� c ����� hTopic; ��������� ����� ���� ��������� ����� OS_GetMessage
   MGetTaskIdByLabel
   task_topic(task_id) = hTopic                                                 ' ������������� ������ ������ WAIT
   task_state(task_id) = OSTS_WAIT                                              ' � �������� ���������� ����������
   goto shedule
end sub
sub OS_SendMessage(byte hTopic , word wMessage)                                 ' ����� ���������� ����� ������� ��������� � ����� hTopic, ���������� �������� ��� �������
    for task_id = 1 to task_count                                               ' ��������� ��� ������
      if task_state(task_id) = OSTS_WAIT then                                   ' ����� �� ������ ������ WAIT?
         if task_topic(task_id) = hTopic then                                   ' ���� �� ����� ������ ��������� �� ���� hTopic?
            os_message(hTopic) = wMessage                                       ' �������� ���������
            task_state(task_id) = OSTS_READY                                    ' ���������� ����� ������ ������ READY
         end if
      end if
   next
 end sub
function OS_GetMessage(byte hTopic) word                                        ' ���������� word ��������� �� ���� hTopic, ����� ���� ������� ���
   OS_GetMessage = os_message(hTopic)
   os_message(hTopic) = NULL
end function
function OS_PeekMessage(byte hTopic)                                            ' ���������� word ��������� �� ���� hTopic, �� �� ������� ���
   OS_PeekMessage = os_message(hTopic)
end function
function OS_GetMessageString(byte hTopic) as string                             ' ���������� ������ ��������� �� ���� hTopic, ����� ���� ������� ���
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
function OS_PeekMessageString(byte hTopic) as string                            ' ���������� ������ ��������� �� ���� hTopic, �� �� ������� ���
   OS_PeekMessageString = ""
   os_tmpword = os_message(hTopic)
   do
      os_tmpbyte = inp(os_tmpword)
      OS_PeekMessageString = OS_PeekMessageString + chr(os_tmpbyte)
      incr os_tmpword
   loop until os_tmpbyte = 0
end function


' --- ������ �������� ��������� ---
function OS_CreateBSemaphore() as byte                                          ' ���������� ���������� ������������� ��������� �������� hBSem
   incr bSem_count
   if bSem_count > 16 then
      OS_RaiseErr OSERR_BSEM_MAX_REACHED                                        ' ���� ������ (��������� ������ ����� ���), ��������� ���������������� ����������
   end if
   OS_CreateBSemaphore = bSem_count - 1
end function
sub OS_WaitBSemaphore(byval hBSem as byte)                                      ' ������������� ������� ������ ��������� "����� ������������ �������� hBSem", ���� ����� (����), ����� ����������
   if bSem_reg.hBSem = TRUE then                                                ' ��������, ����� �� ������� hBSem?
      set task_bsem(task_current).hBSem                                         ' ������������� � task_bsem(������� ������) ��� ����� hBsem
      task_state(task_current) = OSTS_WAIT                                      ' ������������� ������� ������ ������ WAIT
      goto shedule                                                              ' � �������� ���������� ����������
   end if
end sub
sub OS_WaitBSemaphoreTask(bylabel task_label as word , byval hBSem as byte)     ' ������������� �������� ������ ��������� "�����" ������������ �������� hBSem, (����)
   if bSem_reg.hBSem = TRUE then                                                ' ��������, ����� �� ������� hBSem?
      MGetTaskIdByLabel
      set task_bsem(task_id).hBSem                                              ' ������������� � task_bsem(&task_label) ��� ����� hBsem
      task_state(task_id) = OSTS_WAIT                                           ' ������������� ������ &task_label ������ WAIT
      goto shedule                                                              ' � �������� ���������� ����������
   end if
end sub
sub OS_BusyBSemaphore(byval hBSem as byte)                                      ' ������������� ������� hBSem � ��������� "������"
   set bSem_reg.hBSem
end sub
sub OS_FreeBSemaphore(byval hBSem as byte)                                      ' ������������� ������� hBSem � ��������� "��������"
    for task_id = 1 to task_count                                               ' ��������� ��� ������:
      if task_state(task_id) = OSTS_WAIT then                                   ' ����� �� ������ ������ WAIT?
         if task_bsem(task_id).hBsem = TRUE and bSem_reg.hBSem = TRUE then      ' ���� �� ����� ������ ������������ �������� hBSem?
            task_bsem(task_id).hBsem = FALSE                                    ' ����� �������� �������� � ������
            bSem_reg.hBSem = FALSE                                              ' ����� ������ ��������
            task_state(task_id) = OSTS_READY                                    ' ���������� ������ ������ READY
         end if
      end if
   next
end sub

' --- ������ ������� ��������� ---
'function OS_CreateCSemaphore() as byte                                          ' ���������� ���������� ������������� �������� �������� h�Sem
'end function
'sub OS_WaitCSemaphore(byval hCSem as byte , byval bSemValue as byte)            ' ������������� ������� ������ ��������� "�����"  �������� bSemValue �������� h�Sem, (����)
'end sub
'sub OS_WaitCSemaphoreTask(bylabel task_label as word , byval hCSem as byte , byval bSemValue as byte)       ' ������������� �������� ������ ��������� "�����" �������� bSemValue �������� h�Sem, (����)
'end sub
'sub OS_IncCSemaphore(byval hCSem as byte)                                       ' ����������� ������� ������� hCSem �� 1
'end sub
'sub OS_DecCSemaphore(byval hCSem as byte)                                       ' ��������� ������� ������� hCSem �� 1
'end sub
'sub OS_SetCSemaphore(byval hCSem as byte , byval bSemValue as byte)             ' ������������� �������� hCSem �������� bSemValue
'end sub

' --- ��������� ������ ---
sub OS_Init(word error_trap)
   user_err_trap = error_trap                                                   ' �������� ����� ����������������� ����������� ������
   shift user_err_trap , right
   #if OS_SIM = TRUE
      config TIMER0 = counter , prescale = 1                                    ' ��������� �������. ��� ��������� prescale=1
   #else
      config TIMER0 = counter , prescale = 8                                    ' ��������� �������. ��� ����� prescale=8
   #endif
   on timer0 tim0_isr save
   for task_id = 1 to OS_MAX_TASK
      task_queue(task_id) = task_id
   next
   task_current = NULL                                                          ' ������� ������ �� ��������
   task_count = NULL                                                            ' ����� ������������������ ����� ����
   enable interrupts
   disable timer0                                                               ' ��������� ��������� ������
end sub
sub OS_RaiseErr(byte os_err_num)
   if os_err_num <> 0 then
      !STS {RR16},R16
      !LDS R16, {user_err_trap_lo}                                              ' �������� � ���� ����� ������
      !PUSH R16
      !LDS R16, {user_err_trap_hi}
      !PUSH R16
      !LDS R16,{RR16}
      return
      ' �� ����� ���� ����� ������ ���������������� ����������
      ' �������� ��� ������� � �������� ���� �� ������ �+0, �+1, � ����� �������� ������������,
      ' � �������� � �������� ����� �������� � ��������� ��������� �� ������ �� �����������
   end if
end sub
sub OS_Sheduler()
   nop
   shedule:
      disable timer0
      disable interrupts
      nop
   ' ������������� ���������� ������,���� ���� ��������
      if task_current <> 0 then
         if task_state(task_current) = OSTS_RUN then                            ' ���� ������ �� ����� "����������" �������� WAIT, DELAY � �.�.
            task_state(task_current) = OSTS_READY                               ' �� ������ �� ������ -x-STOP-x-  READY (1.05!)
         end if
         if task_state(task_current) <> OSTS_STOP or task_state(task_current) <> OSTS_RESTART then       ' ������ �� �������� ���� ����� ���������� � ������
            ' ��������� �������� ������: ������� �� ����� ����� �������� � ���� �����, ��� ��������� ������� � �������
            !STS {RR16},R16                                                     ' �������� R16 � ������
            !pop R16
            !sts {task_ptr_hi},R16
            !pop R16
            !sts {task_ptr_lo},R16
            !LDS R16,{RR16}                                                     ' ����������� R16
            ' �������� ����� �������� � ����� �������� ������ � �������
            task_adr_lo(task_current) = task_ptr_lo
            task_adr_hi(task_current) = task_ptr_hi
         end if
         if task_state(task_current) = OSTS_RESTART then task_state(task_current) = OSTS_READY
         task_current = 0                                                       ' ������� ������ �� ��������
      end if

   ' *** �������� � �������� ����� ***
     ' ���� ������ � ��������� (����������� �����) ����������� � �������� OSTS_READY
      minprio = 255
      for task_id = 1 to OS_MAX_TASK
         debug "in queue(" ; task_id ; ")=" ; task_queue(task_id)
         if task_state(task_queue(task_id)) <> OSTS_READY then continue
         if task_prior(task_queue(task_id)) < minprio then
            minprio = task_prior(task_queue(task_id))
            minqueuidx = task_id                                                ' ��������� �� ������ �������
         end if
      next

      if minprio > OS_MAX_TASK then
      ' ��� �� ����� ������ �� �������� READY
         enable timer0
         enable interrupts
         nop                                                                    ' ����� ������ ���������� ���������� �������
         goto shedule                                                           ' ����������� ��������, ���� ��� �����
      end if

   ' ������� �� ������� ������������ �������, �������� �������, � �������� ��� � �����
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

   ' �������� ������������ ������
      task_state(task_current) = OSTS_RUN
      task_ptr_lo = task_adr_lo(task_current)
      task_ptr_hi = task_adr_hi(task_current)
      !STS {RR16},R16                                                           ' �������� R16 � ������
      !LDS R16,{task_ptr_lo}                                                    ' �������� � ���� ����� ������
      !PUSH R16
      !LDS R16,{task_ptr_hi}
      !PUSH R16
      !LDS R16,{RR16}                                                           ' ����������� R16
      enable interrupts
      enable timer0                                                             ' �� ������� �������� ������
   return                                                                       ' �� ����� ���� ������� � ������
end sub

' *********************************************************************************************************************
' --- ���������� ���������� ������� ---
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
            decr task_delay(isr_i)                                              ' �������������� �������� � ���������� �����
            if task_delay(isr_i) = 0 then                                       ' ������� �� ��������
               if task_state(isr_i) = OSTS_WAIT then set OS_TIMEOUT
               task_state(isr_i) = OSTS_READY                                   ' ������ ������ ������ READY
            end if
         end if
      next
   end if
   set TIFR0.TOV0
return


RTOS_done: