-module(warehouse).
-export([loop/0]).

loop() -> 
  receive   
    {init, T} 	          -> ets:new(T,[named_table]),                                                   
		             loop(); 
    
    {inquire, I, T, Home} -> case ets:member(T, I) of 
			       false -> Home ! {inquired, T, 0, I},
			                loop();
			       
			       true  -> Answer = ets:lookup(T, I),
				        [{_,C}] = Answer,            
	       	  	                Home ! {inquired, T, C, I},                 
			                loop()
			     end;

 
    {add, T, C, I, Home}  -> case ets:member(T,I) of
			       false -> ets:insert(T,{I, C}),
				        Home ! {added, T, C, I},
				        loop();
 		  	      
			       true  -> NewCount = ets:update_counter(T, I, {2, C}),
				        Home ! {added, T, NewCount, I},
		       		        loop()
			
			     end;

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