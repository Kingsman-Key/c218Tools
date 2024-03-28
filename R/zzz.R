# .onLoad <- function(libname, pkgname)
# {
#   library.dynam("mclust", pkgname, libname)
# }


  msg <- c(paste0(
r"{
 __      __   _                    _        __   __      _         _
 \ \    / ___| |__ ___ _ __  ___  | |_ ___  \ \ / _  _  | |   __ _| |__
  \ \/\/ / -_| / _/ _ | '  \/ -_) |  _/ _ \  \ V | || | | |__/ _` | '_ \
   \_/\_/\___|_\__\___|_|_|_\___|  \__\___/   |_| \_,_| |____\__,_|_.__/

                              %%    %%
                            %@%%%%% %%
                             %%     ,,,,,,,,,,
                             % %  ,,,,,,,@@@@@@@
                                 ,,,,,@@@@@@@@@@@
                              ,  ,,,,,@@@@@@@@@@@
                           ,,,,  ,,,,,,@@@@@@@@@
                      ,,,,,,,,,   ,,,,,,,@@@@@@
                       ,,,,,,,    ,,,@@@@@@@@@
                           ,,,   ,,,,@@@@@@
                              ,,,,@@@@@


                  %%%%%%%%  ,,,,   , ,   ,   %%%%%%%%
                  %%%%%%%%   ,,    , ,   ,   %%%%%%%%
                  %%%%%%%%  ,,,,  ,    ,     %%%%%%%%
Professor Yunxian Yu experts in
Genetic and molecular epidemiology of pregnancy complications
fetal and infant health.
Medical big data mining and application research
Personal profile: https://person.zju.edu.cn/yunxianyu
Email: yunxianyu@zju.edu.cn

}",
    packageVersion("c218Tools")),
    "\nType 'citation(\"c218Tools\")' for citing this R package in publications.")




.onAttach <- function(libname, pkgname) {
  packageStartupMessage(msg, packageVersion(pkgname), pkgname)
}


