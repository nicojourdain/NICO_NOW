PRO std_main
@initenv
;
  type = getenv('PLOTTYPE')
  CASE type OF
    '':print, 'The environment variable PLOTTYPE is not defined. We stop'
    'ts':std_ts_all, /postscript
    'plot':std_plot_all, /postscript
    ELSE:print, 'Wrong definition of the environment variable PLOTTYPE. We stop'
  ENDCASE
;
END
