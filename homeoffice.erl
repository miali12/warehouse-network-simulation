-module(homeoffice).
-export([loop/0]).

loop() ->
  receive
    %initialize table of warehouses.
    startup 		 		   -> io:format("home office started~n"),
			   	 	      ets:new(warehouseTable, [named_table]),
	       	       	    		      loop();
    
    % create a new process(warehouse) and assign W as its name. Add the newly created warehouse to the table of warehouses. 
    {createwh, W} 	 		   -> Wid = spawn(fun warehouse:loop/0),
			   	              Wid ! {init, W},
			    	              ets:insert(warehouseTable, {W, Wid}),
			   		      io:format("warehouse ~p initialized~n", [W]),
			      	              loop();
    
    % For every warehouse currently in the homeoffice's table, ask the warehouse how much quantity of the specified animal it has. 
    {inquire, Animal}   		   -> ListOfWarehouses = ets:tab2list(warehouseTable),
				              lists:map(fun({W, Wid}) -> Wid ! {inquire, Animal, W, self()}, waitForWarehouseMessage() end, ListOfWarehouses),
	  			  	      loop();
    
    % send message to the specified warehouse "W", to add "Amount" many "Animals" to its inventory.   
    {add, W, Amount, Animal}	           -> [{_, Wid}] = ets:lookup(warehouseTable, W),
					      Wid ! {add, W, Amount, Animal, self()},
 			     	   	      waitForWarehouseMessage(),
					      loop();
    
    % Send message to the specified warehouse "W" to sell "Amount" many "Animals" from its inventory. 
    {sell, W, Amount, Animal} 		   -> [{_, Wid}] = ets:lookup(warehouseTable, W),
					      Wid ! {sell, W, Amount, Animal, self()}, 
 			   		      waitForWarehouseMessage(), 
				 	      loop();
    
    % take "amount" many "animals" from warehouse "WFrom" and put them in warehouse "WTo".
    {transfer, WFrom, WTo, Amount, Animal} -> [{_, WidFrom}] = ets:lookup(warehouseTable, WFrom),
					      [{_, WidTo}] = ets:lookup(warehouseTable, WTo),
				 	      WidFrom ! {sell, WFrom, Amount, Animal, self()},
			   		      waitForWarehouseMessage(), 
					      WidTo ! {add, WTo, Amount, Animal, self()},
					      waitForWarehouseMessage(), 
					      loop()  

  end.

% after interacting with the warehouse(s), wait for a confirmation message from the warehouse.
waitForWarehouseMessage() ->
  receive
    {inquired, Warehouse, Count, Animal} -> io:format("warehouse ~p has ~p ~p~n",[Warehouse, Count, Animal]);

    {added, Warehouse, Count, Animal}    -> io:format("warehouse ~p now has ~p ~p~n", [Warehouse, Count, Animal]);
	
    {sold, Warehouse, Count, Animal}     -> io:format("warehouse ~p now has ~p ~p~n", [Warehouse, Count, Animal])

  end.    
