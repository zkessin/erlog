%-*-Prolog-*-

send(Pid,Msg) :-
	ecall(erlog_mailbox:send(Pid,Msg),_).

receive(Msg,Timeout) :-
	ecall(erlog_mailbox:receive_msg(Timeout),Msg).

receive(Msg) :-
	receive(Msg, infinity).

	