library(profvis)
profvis({ runApp('./app.R') }  
        , prof_output = '../')


profvis(prof_input = '../file463c17c730d0.Rprof') 
p <- profvis(prof_input = '/path_to_save_output/file108f93bff877b.Rprof')
htmlwidgets::saveWidget(p, "/path_to_save_output/profile.html")

