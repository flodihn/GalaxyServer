{application,sws_srv,
             [{description,"SWS - Galaxy simulation server"},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib,holonet_srv]},
              {mod,{sws_srv_app,[]}},
              {env,[]},
              {modules,[galaxy_helper,simulation_callback,sws_srv,sws_srv_app,
                        sws_srv_sup]}]}.
