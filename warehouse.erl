-module(warehouse).
-export([loop/0]).

loop() -> 
  receive   
    % initialze the table for this newly created warehouse. 
    {init, T} 	          -> ets:new(T,[named_table]),                                                   
		             loop(); 
    
    % if the specified animal "I" is contained in the table "T" of this warehouse, send message back to 
    % the homeoffice that we have "C" many of "I". Else tell the homeoffice we do not have the animal "I".
    {inquire, I, T, Home} -> case ets:member(T, I) of 
			       false -> Home ! {inquired, T, 0, I},
			                loop();
			       
			       true  -> Answer = ets:lookup(T, I),
				        [{_,C}] = Answer,            
	       	  	                Home ! {inquired, T, C, I},                 
			                loop()
			     end;

    
    % if we don't already have animal "I" then add animal "I" to our table "T".
    % else update the count of animal "I". 
    {add, T, C, I, Home}  -> case ets:member(T,I) of
			       false -> ets:insert(T,{I, C}),
				        Home ! {added, T, C, I},
				        loop();
 		  	      
			       true  -> NewCount = ets:update_counter(T, I, {2, C}),
				        Home ! {added, T, NewCount, I},
		       		        loop()
			
			     end;
    
    % if we get rid of "C" many "I" animals and the resulting quantity of "I" is less than or equal to
    % 0 then send message to homeoffice that we now have "C" less of the "I" animal. Else decrement 
    % the count of animal "I" by "C". (We cannot have negative quantites).
    {sell, T, C, I, Home} -> AnimalCount = ets:lookup(T,I),
			     [{_, Count}] = AnimalCount,
			     if  
			       (Count - C) =< 0 -> ets:insert(T,{I, 0}),
					           Home ! {sold, T, 0, I},
					           loop();
			    
			       true             -> NewCount = ets:update_counter(T, I, {2, (-C)}), 
				                   Home ! {sold, T, NewCount, I},
		        	                   loop()
     			     end

  end.
