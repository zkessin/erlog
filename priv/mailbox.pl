%-*-Prolog-*-

send(Pid,Msg) :-
	ecall(erlog_mailbox:send(Pid,Msg),_).

