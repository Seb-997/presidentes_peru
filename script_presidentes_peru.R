library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(extrafont)
library(stringr)
library(sf)

presidentes <- read.csv('Presidentes_Peru.csv',stringsAsFactors=F , encoding='UTF-8')


# cargando y formateando la data
presidentes <- presidentes%>%
  arrange(-orden)%>%
  mutate(orden2 = row_number(),
         #presidentes que fallecieron en funciones
         Presidente = ifelse(orden %in% c(14,24,42,47,60),paste0(Presidente,'†'),Presidente),
         #formateando fechas
         Inicio_del_mandato = as.Date(Inicio_del_mandato, format = '%d/%m/%Y' ),
         Fin_del_mandato = as.Date(Fin_del_mandato, format = '%d/%m/%Y' ),
         #calculando los dias entre el 28/07/1821 y la fecha de los mandatos
         dias_desde_ind_inicio = as.numeric(difftime(Inicio_del_mandato,min(Inicio_del_mandato), units = "days")),
         dias_desde_ind_fin = as.numeric(difftime(Fin_del_mandato, min(Inicio_del_mandato),units = "days") ),
         dias_de_gob = as.numeric(difftime(Fin_del_mandato, Inicio_del_mandato,units = "days")))


#### PLOT PRESIDENTES SEGUN TOMA DE PODER     


# texto explicativo         
texto <- 'Cada barra representa un periodo de gobierno para cada presidente según la manera en como obtuvieron el cargo. Por ejemplo A. Fujimori aparece 5 veces ya que obtuvo el cargo via elecciones 3 veces (90,95,00), via golpe 1 vez (92), y fue nombrado por la consituyente 1 vez (93). Fuente Wikipedia, Congreso Perú'
texto_wrap <- str_wrap(texto,width=70)

plot<-ggplot()+
  #creado periodos de conflicto (rojo) y bonanza (azul)
  annotate('rect',xmin=0,xmax=max(presidentes$orden2)+2,ymin=5270,ymax=6385,fill='#CC2C00',alpha=.7)+
  annotate('rect',xmin=0,xmax=max(presidentes$orden2)+2,ymin=8667,ymax=16228,fill='#345995',alpha=.7)+
  annotate('rect',xmin=0,xmax=max(presidentes$orden2)+2,ymin=15863 ,ymax=16593 ,fill='#CC2C00',alpha=.7)+
  annotate('rect',xmin=0,xmax=max(presidentes$orden2)+2,ymin=21070,ymax=22896,fill='#CC2C00',alpha=.7)+
  annotate('rect',xmin=0,xmax=max(presidentes$orden2)+2,ymin=43805,ymax=44013,fill='#CC2C00',alpha=.7)+
  annotate('rect',xmin=0,xmax=max(presidentes$orden2)+2,ymin=58001,ymax=64985,fill='#CC2C00',alpha=.7)+
  annotate('rect',xmin=0,xmax=max(presidentes$orden2)+2,ymin=65170,ymax=70284,fill='#345995',alpha=.7)+
  #texto para los periodos
  geom_text(aes(x=87.25,y=5270),label='Guerra de la confederación',hjust=-.01,size=4.3,fontface='bold',family='Thabit-Bold')+
  geom_text(aes(x=86,y=9677),label='Era del guano',hjust=-.01,size=4.3,fontface='bold',family='Thabit-Bold')+
  geom_text(aes(x=84,y=16163),label='Guerra \ncon España',hjust=-.01,size=4.3,fontface='bold',family='Thabit-Bold')+
  geom_text(aes(x=81.5,y=21070),label='Guerra \ncon Chile',hjust=-.01,size=4.3,fontface='bold',family='Thabit-Bold')+
  geom_text(aes(x=85,y=43805),label='Guerra \ncon Ecuador',hjust=-.01,size=4.3,fontface='bold',family='Thabit-Bold')+
  geom_text(aes(x=85,y=58001),label='Terrorismo',hjust=-.01,size=4,fontface='bold',family='Thabit-Bold')+
  geom_text(aes(x=85,y=65170),label='Boom de\nmaterias primas',hjust=-.1,size=4.3,fontface='bold',family='Thabit-Bold')+
  geom_text(aes(x=92.2,y=41805),label=texto_wrap,hjust=0,family='Thabit')+
  #texto y lineas para las anotaciones
  geom_text(aes(y=35835,x=65),label='Primer presidente\nnacido después de la independencia',size=4,fontface='bold',family='Thabit-Bold')+
  geom_curve(aes(y=25694,yend=35835,x=61,xend=64),angle=125,curvature = 0.3,linetype=2)+
  geom_text(aes(x=30,y=26835),label='Periodo\n más largo sin golpe\n1886-1914',fontface='bold',family='Thabit-Bold')+
  geom_segment(aes(x=46,xend=31,y=23486,yend=23486),linetype=2)+
  geom_segment(aes(x=31,xend=35,y=30235,yend=33593),linetype=2)+
  geom_text(aes(x=5,y=59900),label='Segundo periodo\n más largo sin golpe\n1993-2020',fontface='bold',family='Thabit-Bold')+
  geom_segment(aes(x=7,xend=11,y=58074,yend=62580),linetype=2)+
  geom_segment(aes(x=4,xend=2,y=63552,yend=72700),linetype=2)+
  geom_text(aes(x=40,y=51489),label='Leguía fue el presidente que\n gobernó más tiempo.\n15 años en total',fontface='bold',family='Thabit-Bold')+
  geom_curve(aes(y=39295,yend=51489,x=37,xend=38),angle=125,curvature = 0.3,linetype=2)+
  geom_curve(aes(y=41869,yend=51489,x=32,xend=38),angle=125,curvature = 0.3,linetype=2)+
  geom_curve(aes(y=43696,yend=51489,x=31,xend=38),angle=125,curvature = 0.3,linetype=2)+
  geom_curve(aes(y=45522,yend=51489,x=30,xend=38),angle=125,curvature = 0.3,linetype=2)+
  geom_curve(aes(y=45839,yend=51489,x=29,xend=38),angle=125,curvature = 0.3,linetype=2)+
  geom_text(aes(x=61,y=3600),label='Periodo llamado\nLa Anarquía\n1841-1844',fontface='bold',family='Thabit-Bold')+
  geom_curve(aes(y=3600,yend=7512,x=63,xend=70),angle=125,curvature = -0.1,linetype=2)+
  geom_text(aes(x=50,y=46489),label='Iglesias es depuesto\npor Cáceres en una guerra civil',fontface='bold',family='Thabit-Bold')+
  geom_curve(aes(y=30504,yend=46489,x=47,xend=48.7),angle=125,curvature = 0.1,linetype=2)+
  geom_text(aes(x=74,y=29835),label='Primer presidente que\nno era militar.',fontface='bold',family='Thabit-Bold')+
  geom_curve(aes(y=14189,yend=29835,x=72,xend=73),angle=125,curvature = 0.1,linetype=2)+
  geom_text(aes(x=46,y=13033),label='Primer presidente miembro\n de un partido.\n El Partido Civil.',fontface='bold',family='Thabit-Bold')+
  geom_curve(aes(y=18333,yend=13333,x=55.5,xend=48.2),angle=125,curvature = 0.1,linetype=2)+
  geom_text(aes(x=10,y=43089),label='Primera elección donde votan mujeres\n y más de 1 millón de peruanos',fontface='bold',family='Thabit-Bold')+
  geom_curve(aes(y=43089,yend=46816,x=10.8,xend=22),angle=125,curvature = -0.3,linetype=2)+
  geom_text(aes(x=32,y=61900),label='Único presidente asesinado\n en funciones',fontface='bold',family='Thabit-Bold')+
  geom_curve(aes(y=51818,yend=61900,x=27,xend=30.5),angle=125,curvature = 0.3,linetype=2)+
  # rectangulos para los mandatos de cada presidente
  geom_rect(aes(ymin=dias_desde_ind_inicio,ymax=dias_desde_ind_fin,xmin=orden2-.45,xmax=orden2+.45,fill=Origen_del_cargo),size=.25,colour='black',data=presidentes)+
  geom_text(aes(x=orden2,y=dias_desde_ind_fin,label=Presidente),hjust=-.05,size=3,vjust=.4,data=presidentes,family='Thabit-Bold')+
  #formateando el grafico
  scale_y_continuous(breaks=seq(0,73900,3650),labels = seq(1821,2021,10),expand = c(0,0), limits=c(0,73050), 
                     sec.axis = sec_axis( trans=~.,breaks=seq(0,73900,3650),labels = seq(1821,2021,10)))+
  scale_x_continuous(expand=c(0,0))+
  scale_fill_manual(values=c('#0A2463','#FB3640','#F9EEC8','#FFC20A','#9CE7D4','#249478','#52A35E','#FD5E66'),guide=guide_legend(nrow=4))+
  coord_flip(clip='off',xlim=c(0,88))+
  labs(title = 'Presidentes del Perú',subtitle='1821-2020',caption='@sebastienpolis',fill='Origen del cargo')+
  ylab('Año empezando el 28 de julio')+
  theme(
    text = element_text(family='Thabit'),
    plot.background = element_rect(fill='#F5F5F5'),
    plot.margin = margin(1,3.5,1,3.5,unit='cm'),
    plot.title = element_text(size=40),
    plot.subtitle = element_text(size=26,margin=margin(.1,0,1,0,unit='cm')),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(size=.65,linetype=2,colour='grey80'),
    panel.grid.minor.x = element_line(size=.55,linetype=2,colour='grey80'),
    panel.grid = element_blank(),
    axis.text.x = element_text(size=16),
    axis.title.x = element_text(size=24,margin=margin(t=1,b=1,unit='cm')),
    axis.line.x = element_line(size=0.5,colour='black'),
    panel.background = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size=22),
    legend.text = element_text(size=16),
    legend.background = element_rect(fill='#F5F5F5'),
    legend.box.margin = margin(b=.5,unit='cm'),
    plot.caption = element_text(size=28)
  )

ggsave('presidentes_historia.png',plot,width=42,height=59.4,units='cm',dpi=500)



#### PLOT DE PRESIDENTES SEGUN PROFESION

presidentes%>%
  filter(Ocupacion != 'Estudios no finalizados')%>%
  mutate(decada = year(Inicio_del_mandato)-year(Inicio_del_mandato)%%10,
         anios_gob = as.numeric(difftime(Fin_del_mandato, Inicio_del_mandato,units = "days")/365 ))%>%
  group_by(Ocupacion)%>%
  summarise(n=sum(anios_gob))%>%
  ggplot()+
  geom_bar(aes(x=reorder(Ocupacion,-n),y=n,fill=Ocupacion),stat='identity')+
  geom_text(aes(x=reorder(Ocupacion,-n),y=n+2,label=round(n)),stat='identity',family='Thabit',size=5)+
  scale_fill_manual(values=c('#0A2463','#F9EEC8','#FFC20A','#9CE7D4','#249478','#FB3640'),guide=guide_legend(ncol=2))+
  ylab('Años de gobierno')+
  xlab('')+
  labs(title='Perú - Años de gobierno según ocupacion del presidente',subtitle='1821-2021',fill='Ocupación')+
  theme(
    text = element_text(family='Thabit'),
    plot.background = element_rect(fill='#F5F5F5'),
    plot.margin = margin(1,3.5,1,3.5,unit='cm'),
    plot.title = element_text(size=34),
    plot.subtitle = element_text(size=26,margin=margin(.1,0,1,0,unit='cm')),
    axis.text.y = element_text(size=20),
    axis.title.y = element_text(size=24),
    axis.line.y = element_line(size=0.5,colour='black'),
    panel.grid.major.y = element_line(size=.65,linetype=2,colour='grey80'),
    panel.grid = element_blank(),
    axis.text.x = element_text(size=20),
    axis.title.x = element_text(size=24),
    axis.line.x = element_line(size=0.5,colour='black'),
    panel.background = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size=22),
    legend.text = element_text(size=12),
    legend.background = element_rect(fill='#F5F5F5'),
    plot.caption = element_text(size=34)
  )


# Años de gobierno segun partido

presidentes%>%
  filter(Ocupacion != 'Estudios no finalizados')%>%
  mutate(decada = year(Inicio_del_mandato)-year(Inicio_del_mandato)%%10,
         anios_gob = as.numeric(difftime(Fin_del_mandato, Inicio_del_mandato,units = "days")/365 ))%>%
  group_by(Organizacion_politica)%>%
  summarise(n=sum(anios_gob))%>%
  ggplot()+
  geom_bar(aes(x=reorder(Organizacion_politica,-n),y=n,fill=Organizacion_politica),stat='identity')+
  geom_text(aes(x=reorder(Organizacion_politica,-n),y=n+2,label=round(n)),stat='identity',family='Thabit',size=5)+
  scale_fill_manual(values=c('#0A2463','#F9EEC8','#FFC20A','#9CE7D4','#249478','#FB3640',
                             '#0A2463','#F9EEC8','#FFC20A','#9CE7D4','#249478','#FB3640',
                             '#0A2463','#F9EEC8','#FFC20A','#9CE7D4','#249478','#FB3640',
                             '#0A2463','#F9EEC8','#FFC20A','#9CE7D4'),guide=guide_legend(ncol=2))+
  ylab('Años de gobierno')+
  xlab('')+
  labs(title='Perú - Años de gobierno según partido del presidente',subtitle='1821-2021')+
  theme(
    text = element_text(family='Thabit'),
    plot.background = element_rect(fill='#F5F5F5'),
    plot.margin = margin(1,3.5,1,3.5,unit='cm'),
    plot.title = element_text(size=34),
    plot.subtitle = element_text(size=26,margin=margin(.1,0,1,0,unit='cm')),
    axis.text.y = element_text(size=20),
    axis.title.y = element_text(size=24),
    axis.line.y = element_line(size=0.5,colour='black'),
    panel.grid.major.y = element_line(size=.65,linetype=2,colour='grey80'),
    panel.grid = element_blank(),
    axis.text.x = element_text(size=20,angle=315,hjust=.1,vjust=0),
    axis.title.x = element_text(size=24),
    axis.line.x = element_line(size=0.5,colour='black'),
    panel.background = element_blank(),
    legend.position = 'none',
    legend.title = element_text(size=22),
    legend.text = element_text(size=12),
    legend.background = element_rect(fill='#F5F5F5'),
    plot.caption = element_text(size=34)
  )


# Top 10 presidentes por tiempo

presidentes%>%
  group_by(Presidente)%>%
  summarise(n=sum(dias_de_gob)/365)%>%
  arrange(-n)%>%
  top_n(10,n)%>%
  ggplot(aes(x=reorder(Presidente,-n),y=n))+
  geom_bar(stat='identity',fill='#FB3640')+
  labs(title='Perú - Top 10 Presidentes por años de gobierno',subtitle='1821-2021')+  
  ylab('Años de gobierno')+
  xlab('')+
  theme(
    text = element_text(family='Thabit'),
    plot.background = element_rect(fill='#F5F5F5'),
    plot.margin = margin(1,3.5,1,3.5,unit='cm'),
    plot.title = element_text(size=34),
    plot.subtitle = element_text(size=26,margin=margin(.1,0,1,0,unit='cm')),
    axis.text.y = element_text(size=20),
    axis.title.y = element_text(size=24),
    axis.line.y = element_line(size=0.5,colour='black'),
    panel.grid.major.y = element_line(size=.65,linetype=2,colour='grey80'),
    panel.grid = element_blank(),
    axis.text.x = element_text(size=20,angle=315,hjust=.1,vjust=0),
    axis.title.x = element_text(size=24),
    axis.line.x = element_line(size=0.5,colour='black'),
    panel.background = element_blank(),
    legend.position = 'none',
    legend.title = element_text(size=22),
    legend.text = element_text(size=12),
    legend.background = element_rect(fill='#F5F5F5'),
    plot.caption = element_text(size=34)
  )
