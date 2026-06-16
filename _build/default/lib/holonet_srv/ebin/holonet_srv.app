{application,holonet_srv,
             [{description,"Holonet news server using Mnesia"},
              {vsn,"1"},
              {registered,[holonet]},
              {applications,[kernel,stdlib,mnesia]},
              {mod,{holonet_srv_app,[]}},
              {env,[]},
              {modules,[holonet_srv,holonet_srv_app,holonet_srv_sup]}]}.
