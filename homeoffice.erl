-module(homeoffice).
-export([loop/0]).

loop() ->
  receive
    startup 		 		   -> io:format("home office started~n"),
			   	 	      ets:new(warehouseTable, [named_table]),
	       	       	    		      loop();
    
    {createwh, W} 	 		   -> Wid = spawn(fun warehouse:loop/0),
			   	              Wid ! {init, W},
			    	              ets:insert(warehouseTable, {W, Wid}),
			   		      io:format("warehouse ~p initialized~n", [W]),
			      	              loop();
    
    {inquire, Animal}   		   -> ListOfWarehouses = ets:tab2list(warehouseTable),
				              lists:map(fun({W, Wid}) -> Wid ! {inquire, Animal, W, self()}, waitForWarehouseMessage() end, ListOfWarehouses),
	  			  	      loop();
    
    {add, W, Amount, Animal}	           -> [{_, Wid}] = ets:lookup(warehouseTable, W),
					      Wid ! {add, W, Amount, Animal, self()},
 			     	   	      waitForWarehouseMessage(),
					      loop();
    
    {sell, W, Amount, Animal} 		   -> [{_, Wid}] = ets:lookup(warehouseTable, W),
					      Wid ! {sell, W, Amount, Animal, self()}, 
 			   		      waitForWarehouseMessage(), 
				 	      loop();
    
    {transfer, WFrom, WTo, Amount, Animal} -> [{_, WidFrom}] = ets:lookup(warehouseTable, WFrom),
					      [{_, WidTo}] = ets:lookup(warehouseTable, WTo),
				 	      WidFrom ! {sell, WFrom, Amount, Animal, self()},
			   		      waitForWarehouseMessage(), 
					      WidTo ! {add, WTo, Amount, Animal, self()},
					      waitForWarehouseMessage(), 
					      loop()  

  end.

waitForWarehouseMessage() ->
  receive
    {inquired, Warehouse, Count, Animal} -> io:format("warehouse ~p has ~p ~p~n",[Warehouse, Count, Animal]);

    {added, Warehouse, Count, Animal}    -> io:format("warehouse ~p now has ~p ~p~n", [Warehouse, Count, Animal]);
	
    {sold, Warehouse, Count, Animal}     -> io:format("warehouse ~p now has ~p ~p~n", [Warehouse, Count, Animal])

  end.    