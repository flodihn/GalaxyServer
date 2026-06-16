{application,account_srv,
             [{description,"User account server"},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{account_srv_app,[]}},
              {env,[]},
              {modules,[account_mnesia,account_srv,account_srv_app,
                        account_srv_sup,account_util]}]}.
