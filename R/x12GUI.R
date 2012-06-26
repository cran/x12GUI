x12GUI <- function(x12orig,...){
	if(class(x12orig)=="x12Single"){
		xl <- new("x12List")
		xl <- list(x12orig)
		xb <- new("x12Batch",list(x12orig@ts))
		xb@x12List[[1]] <- x12orig
		x12orig <- X12(xb)
	}else if(class(x12orig)=="ts"){
		x12orig <- X12(new("x12Batch",list(x12orig)))
	}
	
	########################################################
	#Variables
	########################################################
	object <- x12orig
	indices <- c(1)
	
	locate <- FALSE
	clickedX <- 100
	
	context.plot1 <- 0
	context.plot2 <- 0
	context.plot3 <- 0
	context.plot4 <- 0
# context.plot5 <- 0
	
	window.main <- gtkWindow("X12 GUI")
	
	notebook.plot <- gtkNotebook()
	statusbar <- gtkStatusbar()
	button.update <- gtkButton("Update")
	
	menubar.main <- gtkMenuBar()
	menuitem.x12update <- gtkMenuItemNewWithMnemonic("_X12 Update")
	menuitem.expplotaspdf <- gtkMenuItem("Export current Plot as PDF")
	menuitem.expplotaspng <- gtkMenuItem("Export current Plot as PNG")
	menuitem.expsummarycsv <- gtkMenuItem("Export summary as CSV")
	menuitem.expsummaryclipboard <- gtkMenuItem("Copy summary to clipboard")
	menuitem.x12loadp <- gtkMenuItem("load Parameter")
	menuitem.x12savep <- gtkMenuItem("save Parameter")
	menuitem.x12load <- gtkMenuItem("load")
	menuitem.x12save <- gtkMenuItem("save")
	menuitem.export <- gtkMenuItem("Export")
	menuitem.x12 <- gtkMenuItem("X12")
	menu.export <- gtkMenu()
	menu.x12 <- gtkMenu()
	
	#ts table
	table.ts <- gtkTreeView()
	renderer.ts <- gtkCellRendererText()
	column.ts <- gtkTreeViewColumn()
	table.model <- gtkListStore("character")
	handler.tstable <- 0
	
	frame.history <- gtkFrame("History")
	panel.history <- gtkVBox()
	label.history <- gtkLabel("revert to:")
	combobox.history <- gtkComboBoxNewText()
	button.revert <- gtkButton("Revert")
	button.clearhistory <- gtkButton("Clean History")
	count.history <- 0
	
	#Panels
	panel.main <- gtkHPaned()
	panel.window <- gtkVBox()
	#Left half of GUI
	panel.gui <- gtkHBox()
	panel.ts <- gtkVBox()
#	panel.params <- gtkVBox()
	panel.params <- gtkTable(60, 6)
	panel.scrolledparams <- gtkScrolledWindow()
	panel.plotp <- gtkVBox()
	
	#plotting frames und areas
	frame.plot1 <- gtkHBox()
	area.plot1 <- gtkDrawingArea()
	frame.plot2 <- gtkHBox()
	area.plot2 <- gtkDrawingArea()
	frame.plot3 <- gtkHBox()
	area.plot3 <- gtkDrawingArea()
	frame.plot4 <- gtkVBox()
	area.plot4 <- gtkDrawingArea()
# frame.plot5 <- gtkHBox()
# area.plot5 <- gtkDrawingArea()
	
	#plotcontextmenu
	menu.contextplotall <- gtkMenu()
	menuitem.saveaspdf <- gtkMenuItemNewWithLabel("Save as PDF")
	
	#plotcontextmenu with manual outlier
	menu.contextplotwithoutlier <- gtkMenu()
	menuitem.saveaspdfwithoutlier <- gtkMenuItemNewWithLabel("Save as PDF")
	menuitem.addAO <- gtkMenuItemNewWithLabel("Add AO Outlier")
	menuitem.addTC <- gtkMenuItemNewWithLabel("Add TC Outlier")
	menuitem.addLS <- gtkMenuItemNewWithLabel("Add LS Outlier")
	
	
	#plotslider
	slider.plotmin <- gtkHScale(min = 0, max= 100, step=5)
	slider.plotmax <- gtkHScale(min = 0, max= 100, step=5)
	
	#summary tab
	frame.summary <- gtkScrolledWindow()
	buffer.summary <- gtkTextBuffer()
	textview.summary <- gtkTextView()
	table.summary <- gtkTreeView()
	columns.summary <- 0
	model.summary <- gtkListStore(rep("character", length(object)+3))
	
	#summarytotal tab
	frame.summarytotal <- gtkScrolledWindow()
	buffer.summarytotal <- gtkTextBuffer()
	textview.summarytotal <- gtkTextView()
	
	#summary parameter
	frame.summaryparameter <- gtkFrame("summary")
	panel.summaryparameter <- gtkVBox()
	checkb.fullSummary <- gtkCheckButtonNewWithLabel("fullSummary")
	checkb.spectraldetail <- gtkCheckButtonNewWithLabel("spectral detail")
	checkb.almostout <- gtkCheckButtonNewWithLabel("almostout")
	checkb.rsdautocorr <- gtkCheckButtonNewWithLabel("rsd autocorr")
	checkb.q2 <- gtkCheckButtonNewWithLabel("q2")
	checkb.likelihoodstat <- gtkCheckButtonNewWithLabel("likelihood stat")
	checkb.aape <- gtkCheckButtonNewWithLabel("aape")
	checkb.idrsdseas <- gtkCheckButtonNewWithLabel("id rsdseas")
	
	#plotparameter panel plot
	panel.scrolledplotparams <- gtkScrolledWindow()
	frame.plotparams <- gtkFrame("Plot")
	panel.plotparams <- gtkVBox()
	checkb.sa <- gtkCheckButtonNewWithLabel("Seasonally Adjusted")
	checkb.trend <- gtkCheckButtonNewWithLabel("Trend")
	checkb.logtransform <- gtkCheckButtonNewWithLabel("Log-Transformation")
	checkb.showAllout <- gtkCheckButtonNewWithLabel("Show Allout")
	frame.showout <- gtkFrame("Show Out")
	checkb.showout <- gtkCheckButton();
	panel.showout <- gtkTable(rows=3, columns=2)
	label.showoutyear <- gtkLabel("year:") 
	entry.showoutyear <- gtkEntry()
	label.showoutperiod <- gtkLabel("period:")
	entry.showoutperiod <- gtkEntry()
	#label.showouttype <- gtkLabel("type:")
	#combobox.showouttype <- gtkComboBoxNewText()
# checkb.showAlloutLines <- gtkCheckButtonNewWithLabel("Show Allout Lines")
# checkb.annComp <- gtkCheckButtonNewWithLabel("AnnComp")
# checkb.annCompTrend <- gtkCheckButtonNewWithLabel("AnnComp Trend")
	
	#plotparameter panel plotFbcast
	frame.plotFbcastparams <- gtkFrame("Plot Fore- & Backcast")
	panel.plotFbcastparams <- gtkVBox()
# checkb.forecast <- gtkCheckButtonNewWithLabel("Forecast")
# checkb.backcast <- gtkCheckButtonNewWithLabel("Backcast")
	checkb.showCI <- gtkCheckButtonNewWithLabel("Show CI")
	checkb.logtransform_fb <- gtkCheckButtonNewWithLabel("Log-Transform")
	#checkb.showLine <- gtkCheckButtonNewWithLabel("Show Line")
	checkb.pointsOriginal <- gtkCheckButtonNewWithLabel("Original Points")
	
	#plotparameter panel plotSeasFac
	frame.plotSeasFacparams <- gtkFrame("Seasonal Factors")
	panel.plotSeasFacparams <- gtkVBox()
	checkb.SIratios <- gtkCheckButtonNewWithLabel("SI-Ratios")
	checkb.SIratiosreplaced <- gtkCheckButtonNewWithLabel("SI-Ratios replaced")
	
	#spectral plot
	frame.spectral <- gtkFrame("spectral")
	panel.spectral <- gtkVBox()
	radiob.spectralsa <- gtkRadioButtonNewWithLabel(label="sa")
	radiob.spectraloriginal <- gtkRadioButtonNewWithLabel("original", group=radiob.spectralsa$GetGroup())
	radiob.spectralirregular <- gtkRadioButtonNewWithLabel("irregular", group=radiob.spectralsa$GetGroup())
	radiob.spectralresiduals <- gtkRadioButtonNewWithLabel("residuals", group=radiob.spectralsa$GetGroup())
	#sa, original, irregular und residuals
	
	#autocorrelation plot
	frame.rsdacf <- gtkFrame("autocorrelation")
	panel.rsdacf <- gtkVBox()
	radiob.rsdacfacf <- gtkRadioButtonNewWithLabel(label="acf")
	radiob.rsdacfpacf <- gtkRadioButtonNewWithLabel("pacf", group=radiob.rsdacfacf$GetGroup())
	radiob.rsdacfacf2 <- gtkRadioButtonNewWithLabel("acf2", group=radiob.rsdacfacf$GetGroup())
	
	
	#manual outliers
	frame.manualoutlier <- gtkFrame("Manual Outliers")
	panel.manualoutlier <- gtkTable(rows=6, columns=2)
	scroll.manualoutlier <- gtkScrolledWindow()
	label.manualoutliertype <- gtkLabel("Type:")
	label.manualoutlieryear <- gtkLabel("Year:")
	label.manualoutlierperiod <- gtkLabel("Period:")
	table.manualoutlier <- gtkTreeView()
	renderer.manualoutliertype <- gtkCellRendererText()
	renderer.manualoutlieryear <- gtkCellRendererText()
	renderer.manualoutlierperiod <- gtkCellRendererText()
	column.manualoutliertype <- gtkTreeViewColumn()
	column.manualoutlieryear <- gtkTreeViewColumn()
	column.manualoutlierperiod <- gtkTreeViewColumn()
	tablemodel.manualoutlier <- gtkListStore("character", "character", "character")
	button.manualoutlierremove <- gtkButton("Remove")
	button.manualoutlieraddclick <- gtkToggleButton("Add by Click")
	button.manualoutlieradd <- gtkButton("Add")
	combobox.manualoutliertype <- gtkComboBoxNewText()
	entry.manualoutlieryear <- gtkEntry()
	entry.manualoutlierperiod <- gtkEntry()
	#List of OutlierLists for each ts, if no manual outliers the list element should be NA
	outlierlist <- list()
	
	#x12 parameters########################
	#span
	checkb.spanactive <- gtkCheckButton()
	frame.span <- gtkFrame("span")
	panel.span <- gtkVBox()
	checkb.spanstart <- gtkCheckButtonNewWithLabel("Start")
	label.spanstartyear <- gtkLabel("Year:")
	entry.spanstartyear <- gtkEntry()
	label.spanstartperiod <- gtkLabel("Period:")
	entry.spanstartperiod <- gtkEntry()
	panel.spanstart <- gtkHBox()
	checkb.spanend <- gtkCheckButtonNewWithLabel("End")
	label.spanendyear <- gtkLabel("Year:")
	entry.spanendyear <- gtkEntry()
	label.spanendperiod <- gtkLabel("Period:")
	entry.spanendperiod <- gtkEntry()
	panel.spanend <- gtkHBox()
	handler.spanstartyear <- 1
	handler.spanstartperiod <- 1
	handler.spanendyear <- 1
	handler.spanendperiod <- 1
	handler.spanactive <- 1
	
	#modelspan
	checkb.modelspanactive <- gtkCheckButton()
	frame.modelspan <- gtkFrame("Modelspan")
	panel.modelspan <- gtkVBox()
	checkb.modelspanstart <- gtkCheckButtonNewWithLabel("Start")
	label.modelspanstartyear <- gtkLabel("Year:")
	entry.modelspanstartyear <- gtkEntry()
	label.modelspanstartperiod <- gtkLabel("Period:")
	entry.modelspanstartperiod <- gtkEntry()
	panel.modelspanstart <- gtkHBox()
	checkb.modelspanend <- gtkCheckButtonNewWithLabel("End")
	label.modelspanendyear <- gtkLabel("Year:")
	entry.modelspanendyear <- gtkEntry()
	label.modelspanendperiod <- gtkLabel("Period:")
	entry.modelspanendperiod <- gtkEntry()
	panel.modelspanend <- gtkHBox()
	handler.modelspanstartyear <- 1
	handler.modelspanstartperiod <- 1
	handler.modelspanendyear <- 1
	handler.modelspanendperiod <- 1
	
	
	#decimals
# checkb.decimalsactive <- gtkCheckButton()
#	panel.decimals <- gtkHBox()
	label.decimals <- gtkLabel("Decimals:")
	entry.decimals <- gtkEntry()
	handler.decimals <- 1
	
	#transform
# checkb.transformactive <- gtkCheckButton()
#	panel.transform <- gtkHBox()
	label.transform <- gtkLabel("Transform:")
	combobox.transform <- gtkComboBoxNewText()
	handler.transform <- 1
	
	#arima
	checkb.arimaactive <- gtkCheckButton()
#	panel.arima <- gtkHBox()
	label.arima <- gtkLabel("Arima:")
	entry.arima1 <- gtkEntry()
	entry.arima2 <- gtkEntry()
	entry.arima3 <- gtkEntry()
	handler.arima1 <- 1
	handler.arima2 <- 1
	handler.arima3 <- 1
	
	#sarima
	checkb.sarimaactive <- gtkCheckButton()
#	panel.sarima <- gtkHBox()
	label.sarima <- gtkLabel("SArima:")
	entry.sarima1 <- gtkEntry()
	entry.sarima2 <- gtkEntry()
	entry.sarima3 <- gtkEntry()
	handler.sarima1 <- 1
	handler.sarima2 <- 1
	handler.sarima3 <- 1
	
	#div checkboxes
	frame.divboxes <- gtkFrame("Settings")
	panel.divboxes <- gtkVBox()
	checkb.automdl <- gtkCheckButtonNewWithLabel("Automdl")
	checkb.acceptdefault <- gtkCheckButtonNewWithLabel("Acceptdefault")
	checkb.balanced <- gtkCheckButtonNewWithLabel("Balanced")
	checkb.seats <- gtkCheckButtonNewWithLabel("Seats")
	checkb.estimate <- gtkCheckButtonNewWithLabel("Estimate")
	checkb.estOutofsample <- gtkCheckButtonNewWithLabel("Estimate out of Sample")
	checkb.slidingspans <- gtkCheckButtonNewWithLabel("Sliding Spans")
	checkb.onlytd <- gtkCheckButtonNewWithLabel("OnlyTd")
	checkb.sfshort <- gtkCheckButtonNewWithLabel("sfshort")
	checkb.x11appendfcst <- gtkCheckButtonNewWithLabel("x11appendfcst")
	checkb.x11appendbcst <- gtkCheckButtonNewWithLabel("x11appendbcst")
	checkb.x11excludefcst <- gtkCheckButtonNewWithLabel("x11excludefcst")
	checkb.x11regress <- gtkCheckButtonNewWithLabel("x11regress")
# checkb.keep_x12out <- gtkCheckButtonNewWithLabel("keep x12out")
# checkb.showWarnings <- gtkCheckButtonNewWithLabel("showWarnings")
	handlercheckb.automdl <- 1
	handlercheckb.acceptdefault <- 1
	handlercheckb.balanced <- 1
	handlercheckb.seats <- 1
	handlercheckb.estimate <- 1
	handlercheckb.estOutofsample <- 1
	handlercheckb.slidingspans <- 1
	handlercheckb.onlytd <- 1
	handlercheckb.sfshort <- 1
	handlercheckb.x11appendfcst <- 1
	handlercheckb.x11appendbcst <- 1
	handlercheckb.x11excludefcst <- 1
	handlercheckb.x11regress <- 1
	
	#maxorder
	checkb.maxorderactive <- gtkCheckButton()
#	panel.maxorder <- gtkHBox()
	label.maxorder <- gtkLabel("Maxorder:")
	entry.maxorder1 <- gtkEntry()
	entry.maxorder2 <- gtkEntry()
	handler.maxorder1 <- 1
	handler.maxorder2 <- 1
	
	#maxdiff
	checkb.maxdiffactive <- gtkCheckButton()
#	panel.maxdiff <- gtkHBox()
	label.maxdiff <- gtkLabel("Maxdiff:")
	entry.maxdiff1 <- gtkEntry()
	entry.maxdiff2 <- gtkEntry()
	handler.maxdiff1 <- 1
	handler.maxdiff2 <- 1
	
	#critical
	checkb.criticalactive <- gtkCheckButton()
	frame.critical <- gtkFrame("critical")
	panel.critical <- gtkTable(rows=4, columns=5)
	radiob.criticalall <- gtkRadioButtonNewWithLabel(label="all")
	radiob.criticalspecific <- gtkRadioButtonNewWithLabel(group = radiob.criticalall$GetGroup(), label="specific")
	entry.criticalall <- gtkEntry()
	entry.criticalAO <- gtkEntry()
	entry.criticalLS <- gtkEntry()
	entry.criticalTC <- gtkEntry()
	label.criticalAO <- gtkLabel("AO")
	label.criticalLS <- gtkLabel("LS")
	label.criticalTC <- gtkLabel("TC")
	handler.criticalall <- 1
	handler.criticalAO <- 1
	handler.criticalTC <- 1
	handler.criticalLS <- 1
	
	#regvariables
	checkb.regvariablesactive <- gtkCheckButton()
#	panel.regvariables <- gtkHBox()
	label.regvariables <- gtkLabel("Regvariables:")
	entry.regvariables <- gtkEntry()
	handler.regvariables <- 1
	
	#reguser
	checkb.reguseractive <- gtkCheckButton()
#	panel.reguser <- gtkHBox()
	label.reguser <- gtkLabel("Reguser:")
	entry.reguser <- gtkEntry()
	handler.reguser <- 1
	
	#regfile
	checkb.regfileactive <- gtkCheckButton()
#	panel.regfile <- gtkHBox()
	label.regfile <- gtkLabel("Regfile:")
	filebutton.regfile <- gtkFileChooserButton("RegFile","GTK_FILE_CHOOSER_ACTION_OPEN")
	
	#usertype
	checkb.usertypeactive <- gtkCheckButton()
#	panel.usertype <- gtkHBox()
	label.usertype <- gtkLabel("usertype:")
	entry.usertype <- gtkEntry()
	handler.usertype <- 1
	
	#centeruser
	checkb.centeruseractive <- gtkCheckButton()
#	panel.centeruser <- gtkHBox()
	label.centeruser <- gtkLabel("centeruser:")
	combobox.centeruser <- gtkComboBoxNewText()
	handler.centeruser <- 1
	
	#regfilestart
	checkb.regfilestartactive <- gtkCheckButton()
	frame.regfilestart <- gtkFrame("RegFileStart")
	panel.regfilestart <- gtkVBox()
	label.regfilestartstartyear <- gtkLabel("Year:")
	entry.regfilestartstartyear <- gtkEntry()
	label.regfilestartstartperiod <- gtkLabel("Period:")
	entry.regfilestartstartperiod <- gtkEntry()
	panel.regfilestartstart <- gtkHBox()
	handler.regfilestartstartyear <- 1
	handler.regfilestartstartperiod <- 1
	
# #tblnames 
# checkb.tblnamesactive <- gtkCheckButton()
# panel.tblnames <- gtkHBox()
# label.tblnames <- gtkLabel("tblnames:")
# entry.tblnames <- gtkEntry()
# 
# #Rtblnames 
# checkb.Rtblnamesactive <- gtkCheckButton()
# panel.Rtblnames <- gtkHBox()
# label.Rtblnames <- gtkLabel("Rtblnames:")
# entry.Rtblnames <- gtkEntry()
# 
# #x12path
# checkb.x12pathactive <- gtkCheckButton()
# panel.x12path <- gtkHBox()
# label.x12path <- gtkLabel("x12path:")
# filebutton.x12path <- gtkFileChooserButton("x12path","GTK_FILE_CHOOSER_ACTION_OPEN")
# 
# #x13path
# checkb.x13pathactive <- gtkCheckButton()
# panel.x13path <- gtkHBox()
# label.x13path <- gtkLabel("x13path:")
# filebutton.x13path <- gtkFileChooserButton("x13path","GTK_FILE_CHOOSER_ACTION_OPEN")
# 
# #use
# checkb.useactive <- gtkCheckButton()
# panel.use <- gtkHBox()
# label.use <- gtkLabel("use:")
# combobox.use <- gtkComboBoxNewText()
	
	#seatsparameter
	checkb.seatsparameteractive <- gtkCheckButton()
#	panel.seatsparameter <- gtkHBox()
	label.seatsparameter <- gtkLabel("seatsparameter:")
	entry.seatsparameter <- gtkEntry()
	handler.seatsparameter <- 1
	
	#sigmalim
	checkb.sigmalimactive <- gtkCheckButton()
#	panel.sigmalim <- gtkHBox()
	label.sigmalim <- gtkLabel("sigmalim:")
	entry.sigmalim1 <- gtkEntry()
	entry.sigmalim2 <- gtkEntry()
	handler.sigmalim1 <- 1
	handler.sigmalim2 <- 1
	
	#outlier
	checkb.outlieractive <- gtkCheckButton()
#	panel.outlier <- gtkHBox()
	label.outlier <- gtkLabel("outlier-detection:")
	frame.outlier <- gtkFrame("outlier-detection")
	panel.outlier <- gtkTable(rows=3, columns=4)
	checkb.outlierall <- gtkCheckButton()
	checkb.outlierTC <- gtkCheckButton()
	checkb.outlierAO <- gtkCheckButton()
	checkb.outlierLS <- gtkCheckButton()
#	entry.outlier <- gtkEntry()
	handler.outlier <- 1
	handler.outlierall <- 1
	handler.outlierAO <- 1
	handler.outlierLS <- 1
	handler.outlierTC <- 1
	
	#outlierspan
	checkb.outlierspanactive <- gtkCheckButton()
#	panel.outlierspan <- gtkHBox()
	label.outlierspan <- gtkLabel("outlierspan:")
	entry.outlierspan1 <- gtkEntry()
	entry.outlierspan2 <- gtkEntry()
	handler.outlierspan1 <- 1
	handler.outlierspan2 <- 1
	
	#outliermethod
	checkb.outliermethodactive <- gtkCheckButton()
#	panel.outliermethod <- gtkHBox()
	label.outliermethod <- gtkLabel("outliermethod:")
	combobox.outliermethod <- gtkComboBoxNewText()
	handler.outliermethod <- 1
	
# #file
# checkb.fileactive <- gtkCheckButton()
# panel.file <- gtkHBox()
# label.file <- gtkLabel("file:")
# entry.file <- gtkEntry()
	
	#forecast_years
	checkb.forecast_yearsactive <- gtkCheckButton()
#	panel.forecast_years <- gtkHBox()
	label.forecast_years <- gtkLabel("forecast_years:")
	entry.forecast_years <- gtkEntry()
	handler.forecast_years <- 1
	
	#backcast_years
	checkb.backcast_yearsactive <- gtkCheckButton()
#	panel.backcast_years <- gtkHBox()
	label.backcast_years <- gtkLabel("backcast_years:")
	entry.backcast_years <- gtkEntry()
	handler.backcast_years <- 1
	
	#forecast_conf
# checkb.forecast_confactive <- gtkCheckButton()
#	panel.forecast_conf <- gtkHBox()
	label.forecast_conf <- gtkLabel("forecast_conf:")
	entry.forecast_conf <- gtkEntry()
	handler.forecast_conf <- 1
	
	#aictest
	checkb.aictestactive <- gtkCheckButton()
#	panel.aictest <- gtkHBox()
	label.aictest <- gtkLabel("aictest:")
	entry.aictest <- gtkEntry()
	handler.aictest <- 1
	
	#samode
	checkb.samodeactive <- gtkCheckButton()
#	panel.samode <- gtkHBox()
	label.samode <- gtkLabel("samode:")
	combobox.samode <- gtkComboBoxNewText()
	handler.samode <- 1
	
	#seasonalma
	checkb.seasonalmaactive <- gtkCheckButton()
#	panel.seasonalma <- gtkHBox()
	label.seasonalma <- gtkLabel("seasonalma:")
	entry.seasonalma <- gtkEntry()
	handler.seasonalma <- 1
	
	#trendma
	checkb.trendmaactive <- gtkCheckButton()
#	panel.trendma <- gtkHBox()
	label.trendma <- gtkLabel("trendma:")
	entry.trendma <- gtkEntry()
	handler.trendma <- 1
	
	#x11calendarsigma
	checkb.x11calendarsigmaactive <- gtkCheckButton()
#	panel.x11calendarsigma <- gtkHBox()
	label.x11calendarsigma <- gtkLabel("x11calendarsigma:")
	combobox.x11calendarsigma <- gtkComboBoxNewText()
	handler.x11calendarsigma <- 1
	
	#x11final
# checkb.x11finalactive <- gtkCheckButton()
#	panel.x11final <- gtkHBox()
	label.x11final <- gtkLabel("x11final:")
	entry.x11final <- gtkEntry()
	handler.x11final <- 1
	
	####X12TOOLTIPS
	string.span <- "text <i>kursiv</i><b>fett</b>"
	string.modelspan <- "text <i>kursiv</i><b>fett</b>"
	string.decimals <- "text <i>kursiv</i><b>fett</b>"
	string.transform <- "text <i>kursiv</i><b>fett</b>"
	string.arima <- "text <i>kursiv</i><b>fett</b>"
	string.sarima <- "text <i>kursiv</i><b>fett</b>"
	string.automdl <- "text <i>kursiv</i><b>fett</b>"
	string.acceptdefault <- "text <i>kursiv</i><b>fett</b>"
	string.balanced <- "text <i>kursiv</i><b>fett</b>"
	string.seats <- "text <i>kursiv</i><b>fett</b>"
	string.estimate <- "text <i>kursiv</i><b>fett</b>"
	string.estimateoutofsamples <- "text <i>kursiv</i><b>fett</b>"
	string.slidingspans <- "text <i>kursiv</i><b>fett</b>"
	string.onlytd <- "text <i>kursiv</i><b>fett</b>"
	string.sfshort <- "text <i>kursiv</i><b>fett</b>"
	string.x11appendfcst <- "text <i>kursiv</i><b>fett</b>"
	string.x11appendfbst <- "text <i>kursiv</i><b>fett</b>"
	string.x11excludefcst <- "text <i>kursiv</i><b>fett</b>"
	string.x11regress <- "text <i>kursiv</i><b>fett</b>"
	string.maxorder <- "text <i>kursiv</i><b>fett</b>"
	string.maxdiff <- "text <i>kursiv</i><b>fett</b>"
	string.regvariables <- "text <i>kursiv</i><b>fett</b>"
	string.reguser <- "text <i>kursiv</i><b>fett</b>"
	string.regfile <- "text <i>kursiv</i><b>fett</b>"
	string.usertype <- "text <i>kursiv</i><b>fett</b>"
	string.centeruser <- "text <i>kursiv</i><b>fett</b>"
	string.regfilestart <- "text <i>kursiv</i><b>fett</b>"
	string.seatsparameter <- "text <i>kursiv</i><b>fett</b>"
	string.sigmalim <- "text <i>kursiv</i><b>fett</b>"
	string.critical <- "text <i>kursiv</i><b>fett</b>"
	string.outlier <- "text <i>kursiv</i><b>fett</b>"
	string.outlierspan <- "text <i>kursiv</i><b>fett</b>"
	string.outliermethod <- "text <i>kursiv</i><b>fett</b>"
	string.forecast_years <- "text <i>kursiv</i><b>fett</b>"
	string.backcast_years <- "text <i>kursiv</i><b>fett</b>"
	string.forecast_conf <- "text <i>kursiv</i><b>fett</b>"
	string.aictest <- "text <i>kursiv</i><b>fett</b>"
	string.samode <- "text <i>kursiv</i><b>fett</b>"
	string.seasonalma <- "text <i>kursiv</i><b>fett</b>"
	string.trendma <- "text <i>kursiv</i><b>fett</b>"
	string.x11calendarsigma <- "text <i>kursiv</i><b>fett</b>"
	string.x11final <- "text <i>kursiv</i><b>fett</b>"
	
	####END Variables###################################
	
	####################################################
	#FUNCTIONS
	####################################################
	#Handler function for the notepad with the plots
	notebookhandler <- function(notebook, page, page.num, user.data){
		update_notebook(page.num)
	}
	
	#Handler function for the NULL-checkboxes of the x12 panel 
	checkbuttonhandler <- function(togglebutton, user_data){
		update_toggle(button=togglebutton, system=FALSE)
	}
	
	#Handler function for the checkbuttons for the boolean x12 values
	checkbuttonx12handler <- function(togglebutton, user_data){
		#automdl
		if(togglebutton == checkb.automdl){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(automdl=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(automdl=FALSE),s)})
			status_print("automdl changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#acceptdefault
		if(togglebutton == checkb.acceptdefault){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(acceptdefault=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(acceptdefault=FALSE),s)})
			status_print("acceptdefault changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#balanced
		if(togglebutton == checkb.balanced){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(balanced=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(balanced=FALSE),s)})
			status_print("balanced changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#seats
		if(togglebutton == checkb.seats){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(seats=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(seats=FALSE),s)})
			status_print("seats changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#estimate
		if(togglebutton == checkb.estimate){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(estimate=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(estimate=FALSE),s)})
			status_print("estimate changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#estOutofsample
		if(togglebutton == checkb.estOutofsample){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(estOutofsample=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(estOutofsample=FALSE),s)})
			status_print("estOutofsample changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#slidingspans
		if(togglebutton == checkb.slidingspans){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(slidingspans=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(slidingspans=FALSE),s)})
			status_print("slidingspans changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#onlytd
		if(togglebutton == checkb.onlytd){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(onlytd=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(onlytd=FALSE),s)})
			status_print("onlytd changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#sfshort
		if(togglebutton == checkb.sfshort){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(sfshort=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(sfshort=FALSE),s)})
			status_print("sfshort changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#x11appendfcst
		if(togglebutton == checkb.x11appendfcst){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(x11appendfcst=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(x11appendfcst=FALSE),s)})
			status_print("x11appendfcst changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#x11appendbcst
		if(togglebutton == checkb.x11appendbcst){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(x11appendbcst=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(x11appendbcst=FALSE),s)})
			status_print("x11appendbcst changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#x11excludefcst
		if(togglebutton == checkb.x11excludefcst){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(x11excludefcst=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(x11excludefcst=FALSE),s)})
			status_print("x11excludefcst changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#x11regress
		if(togglebutton == checkb.x11regress){
			if(togglebutton$GetActive()==TRUE)lapply(indices, FUN=function(s){object <<- setP(object,list(x11regress=TRUE),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(x11regress=FALSE),s)})
			status_print("x11regress changed!")
			togglebutton$SetInconsistent(FALSE)
		}
		#outlier
		if(togglebutton == checkb.outlierall || togglebutton == checkb.outlierAO ||
				togglebutton == checkb.outlierLS ||togglebutton == checkb.outlierTC){
			if(togglebutton == checkb.outlierall)toggle(c(checkb.outlierAO, checkb.outlierLS, checkb.outlierTC),checkb.outlierall, invert=TRUE)
			checkb.outlierall$SetInconsistent(FALSE)
			checkb.outlierAO$SetInconsistent(FALSE)
			checkb.outlierTC$SetInconsistent(FALSE)
			checkb.outlierLS$SetInconsistent(FALSE)
			checkb.outlieractive$SetInconsistent(FALSE)
			i <- numeric(0)
			if(checkb.outlierAO$GetActive()==TRUE)i <- append(i, "AO")
			if(checkb.outlierLS$GetActive()==TRUE)i <- append(i, "LS")
			if(checkb.outlierTC$GetActive()==TRUE)i <- append(i, "TC")
			if(checkb.outlierall$GetActive()==TRUE)i <- "all"
			if(length(i)>0)lapply(indices, FUN=function(s){object <<- setP(object,list(outlier=i),s)})
			else lapply(indices, FUN=function(s){object <<- setP(object,list(outlier=NULL),s)})
		}
	}
	
	comboboxx12handler <- function(widget, user_data){
		#transform
		if(widget == combobox.transform){
			if(widget$GetActive()==0)lapply(indices, FUN=function(s){object <<- setP(object,list(transform="auto"),s)})
			if(widget$GetActive()==1)lapply(indices, FUN=function(s){object <<- setP(object,list(transform="log"),s)})
			if(widget$GetActive()==2)lapply(indices, FUN=function(s){object <<- setP(object,list(transform="none"),s)})
			status_print("transform changed!")
		}
		
		#centeruser
		if(widget == combobox.centeruser){
			if(widget$GetActive()==0)lapply(indices, FUN=function(s){object <<- setP(object,list(centeruser="mean"),s)})
			if(widget$GetActive()==1)lapply(indices, FUN=function(s){object <<- setP(object,list(centeruser="seasonal"),s)})
			status_print("centeruser changed!")
		}
		
		#outlier_method
		if(widget == combobox.outliermethod){
			if(widget$GetActive()==0)lapply(indices, FUN=function(s){object <<- setP(object,list(outlier_method="addone"),s)})
			if(widget$GetActive()==1)lapply(indices, FUN=function(s){object <<- setP(object,list(outlier_method="addall"),s)})
			status_print("outlier_method changed!")
		}
		
		#samode
		if(widget == combobox.samode){
			if(widget$GetActive()==0)lapply(indices, FUN=function(s){object <<- setP(object,list(samode="mult"),s)})
			if(widget$GetActive()==1)lapply(indices, FUN=function(s){object <<- setP(object,list(samode="add"),s)})
			if(widget$GetActive()==2)lapply(indices, FUN=function(s){object <<- setP(object,list(samode="pseudoadd"),s)})
			if(widget$GetActive()==3)lapply(indices, FUN=function(s){object <<- setP(object,list(samode="logadd"),s)})
			status_print("samode changed!")
		}
		
		#x11calendarsigma
		if(widget == combobox.x11calendarsigma){
			if(widget$GetActive()==0)lapply(indices, FUN=function(s){object <<- setP(object,list(x11calendarsigma="all"),s)})
			if(widget$GetActive()==1)lapply(indices, FUN=function(s){object <<- setP(object,list(x11calendarsigma="signif"),s)})
			if(widget$GetActive()==2)lapply(indices, FUN=function(s){object <<- setP(object,list(x11calendarsigma="select"),s)})
			status_print("x11calendarsigma changed!")
		}
	}
	
	#Handler for timeseries table, called after new selection occurred
	#retrieves selection indices und uses them for updating plots and x12 parametergui
	tablehandler <- function(treeselection, userdata,...){
		indices <<- sapply(treeselection$GetSelectedRows()$retval, FUN=function(s){s$GetIndices()})+1
#		print(indices)
		update_notebook()
		make_history()
		status_print(capture.output(read_x12(object, indices)))	
#		read_x12(object, indices)
		update_outliertable()
	}
	
	#Handler responsible for the mouseclicks on plot surfaces
	mousehandlerdrawing <- function(widget,event,userdata){
		if(widget == area.plot1){
			if(event$button == 3){
				menu.contextplotall$Popup(button=3, activate.time=0)
			}
		}
		if(widget == area.plot2){
			if(event$button == 3){
				menu.contextplotall$Popup(button=3, activate.time=0)
			}
		}
		if(widget == area.plot3){
			if(event$button == 3){
				menu.contextplotall$Popup(button=3, activate.time=0)
			}
		}
		if(widget == area.plot4){
			if(event$button == 1){
				if(locate == TRUE){
					locate <<-  FALSE
					button.manualoutlieraddclick$SetActive(FALSE)
					oc <- convertCoordinates(event$x)
#					offset <- floor(oc*(slider.plotmax$GetValue()-slider.plotmin$GetValue())+slider.plotmin$GetValue())
#					if(offset < 0)offset <- 0
#					t <- times(object@x12List[[indices[1]]])
#					year <- 0
#					period <- 0
#					if(!is.null(t$backcast)){
#						year <- (t$backcast[1]+floor(offset/frequency(object@x12List[[indices[1]]]@ts)))
#						period <- ((t$backcast[2]-1+offset)%%frequency(object@x12List[[indices[1]]]@ts)+1)
#					}
#					else{
#						year <- (t$original[1]+floor(offset/frequency(object@x12List[[indices[1]]]@ts)))
#						period <- ((t$original[2]-1+offset)%%frequency(object@x12List[[indices[1]]]@ts)+1)
#					}
					entry.manualoutlieryear$SetText(oc$year)
					entry.manualoutlierperiod$SetText(oc$period)
				}
#				print(convertCoordinates(event$x))
			}
			if(event$button == 3){
				clickedX <<- event$x
				menu.contextplotwithoutlier$Popup(button=3, activate.time=0)
			}
		}
	}
	
	#Trys to Interpolate pixelcoordinates/usercoordinates to timeseries-values
	#could use some tweaking
	#######################################
	convertCoordinates <- function(x){
		click <- x/area.plot4$GetAllocation()$allocation$width
		usract <- par("usr")[1:2]
		pltact <- par("plt")[1:2]
		xnew <- (((click -pltact[1])/(pltact[2] -pltact[1]))*(usract[2] -usract[1]))+usract[1]
		start_year <- floor(usract[1])
		end_year <- ceiling(usract[2])
		mm <- (0:(frequency(object@x12List[[indices]]@ts)-1))/frequency(object@x12List[[indices]]@ts)
		start_year:end_year
		tt <- expand.grid(start_year:end_year,mm)
		tt <- tt[,1]+tt[,2]
		tt <- tt[which.min(abs(xnew-tt))]
		year <- floor(tt)
		month <- round(1+(tt-year)*frequency(object@x12List[[indices]]@ts))
		timess <- time(object@x12List[[indices]]@ts)
		timeend <- timess[length(timess)]
		timestart <- timess[1]
		time <- timeend[length(timeend)]
		if(tt>timeend){
			ee <- end(object@x12List[[indices]]@ts)
			year <- ee[1]
			month <- ee[2]
		}else if(tt<timestart){
			ss <- start(object@x12List[[indices]]@ts)
			year <- ss[1]
			month <- ss[2]
		}
		
		
		ret <- list(year=year, period=month)
	}
	#######################################
	
	#reads values of the manualoutlierlist into the corresponding databuffer 
	update_outliertable <- function(){
		tablemodel.manualoutlier$Clear()
		if(length(outlierlist)>0){
			sapply(outlierlist,
					function(string) {
						## Add a new row to the model
						iter <- tablemodel.manualoutlier$Append()$iter
						tablemodel.manualoutlier$Set(iter, 0, string[1], 1, string[2], 2, string[3])
					})
		}
	}
	
	#encapsulates all NULL-checkbox updates in one function for simplified usage
	update_toggle <- function(button=NULL, system=TRUE){
		if(button==checkb.spanactive){
			toggle(c(entry.spanstartyear, entry.spanstartperiod, entry.spanendyear, 
							entry.spanendperiod, checkb.spanstart, checkb.spanend), button)
			if(button$GetActive()==FALSE){
				lapply(indices, FUN=function(s){object <<- setP(object,list(span=NULL),s)})
				button$SetInconsistent(FALSE)
				status_print("span changed!")
			}
			else{
				gSignalEmit(entry.spanstartyear, "changed")
			}
		}
		if(button==checkb.modelspanactive){
			toggle(c(entry.modelspanstartyear, entry.modelspanstartperiod, entry.modelspanendyear, 
							entry.modelspanendperiod, checkb.modelspanstart, checkb.modelspanend), button)
			if(button$GetActive()==FALSE){
				lapply(indices, FUN=function(s){object <<- setP(object,list(modelspan=NULL),s)})
				button$SetInconsistent(FALSE)
				status_print("modelspan changed!")
			}
			else{
				gSignalEmit(entry.modelspanstartyear, "changed")
			}
		}
		if(button==checkb.spanstart){
			toggle(c(entry.spanstartyear, entry.spanstartperiod), button)
			gSignalEmit(entry.spanstartyear, "changed")
		}
		if(button==checkb.spanend){
			toggle(c(entry.spanendyear, entry.spanendperiod), button)
			gSignalEmit(entry.spanstartyear, "changed")
		}
		if(button==checkb.modelspanstart){
			toggle(c(entry.modelspanstartyear, entry.modelspanstartperiod), button)
			gSignalEmit(entry.modelspanstartyear, "changed")
		}
		if(button==checkb.modelspanend){
			toggle(c(entry.modelspanendyear, entry.modelspanendperiod), button)
			gSignalEmit(entry.modelspanstartyear, "changed")
		}
#   if(button==checkb.decimalsactive){
#     toggle(c(entry.decimals), button)
#   }
#   if(button==checkb.transformactive){
#     toggle(c(combobox.transform), button)
#   }
		if(button==checkb.arimaactive){
			toggle(c(entry.arima1, entry.arima2, entry.arima3), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(arima=NULL),s)})
				status_print("arima changed!")
			}
			else{
				gSignalEmit(entry.arima1, "changed")
			}
		}
		if(button==checkb.sarimaactive){
			toggle(c(entry.sarima1, entry.sarima2, entry.sarima3), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(sarima=NULL),s)})
				status_print("sarima changed!")
			}
			else{
				gSignalEmit(entry.sarima1, "changed")
			}
		}
#   if(button==checkb.maxorderactive){
#     toggle(c(entry.maxorder1, entry.maxorder2), button)
#   }
#   if(button==checkb.maxdiffactive){
#     toggle(c(entry.maxdiff1, entry.maxdiff2), button)
#   }
		if(button==checkb.regvariablesactive){
			toggle(c(entry.regvariables), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(regvariables=NULL),s)})
				status_print("regvariables changed!")
			}
			else{
				gSignalEmit(entry.regvariables, "changed")
			}
		}
		if(button==checkb.reguseractive){
			toggle(c(entry.reguser), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(reguser=NULL),s)})
				status_print("reguser changed!")
			}
			else{
				gSignalEmit(entry.reguser, "changed")
			}
		}
		if(button==checkb.regfileactive){
			toggle(c(filebutton.regfile), button)
		}
		
		if(button==checkb.usertypeactive){
			toggle(c(entry.usertype), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(usertype=NULL),s)})
				status_print("usertype changed!")
			}
			else{
				gSignalEmit(entry.usertype, "changed")
			}
		}
		if(button==checkb.centeruseractive){
			toggle(c(combobox.centeruser), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(centeruser=NULL),s)})
				status_print("centeruser changed!")
			}
			else{
				
				gSignalEmit(combobox.centeruser, "changed")
			}
		}
		if(button==checkb.regfilestartactive){
			toggle(c(entry.regfilestartstartyear, entry.regfilestartstartperiod), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(regfilestart=NULL),s)})
				status_print("regfilestart changed!")
			}
			else{
				gSignalEmit(entry.regfilestartstartyear, "changed")
			}
		}
#   if(button==checkb.tblnamesactive){
#     toggle(c(entry.tblnames), button)
#   }
#   if(button==checkb.Rtblnamesactive){
#     toggle(c(entry.Rtblnames), button)
#   }
#   if(button==checkb.x12pathactive){
#     toggle(c(filebutton.x12path), button)
#   }
#   if(button==checkb.x13pathactive){
#     toggle(c(filebutton.x13path), button)
#   }
#   if(button==checkb.useactive){
#     toggle(c(combobox.use), button)
#   }
		if(button==checkb.seatsparameteractive){
			toggle(c(entry.seatsparameter), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(seatsparameter=NULL),s)})
				status_print("seatsparameter changed!")
			}
			else{
				gSignalEmit(entry.seatsparameter, "changed")
			}
		}
		if(button==checkb.sigmalimactive){
			toggle(c(entry.sigmalim1, entry.sigmalim2), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(sigmalim=NULL),s)})
				status_print("sigmalim changed!")
			}
			else{
				gSignalEmit(entry.sigmalim1, "changed")
			}
		}
		if(button==checkb.criticalactive){
			toggle(c(radiob.criticalall, radiob.criticalspecific), button)
			gSignalEmit(radiob.criticalall, "toggled")
			gSignalEmit(radiob.criticalspecific, "toggled")
			if(button$GetActive()==FALSE){
				toggle(c(entry.criticalAO, entry.criticalTC, entry.criticalLS, 
								radiob.criticalspecific,entry.criticalall, radiob.criticalall), checkb.criticalactive)
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(critical=NULL),s)})
				status_print("critical changed!")
			}
		}
		if(button==radiob.criticalall){
			if(button$GetActive()==TRUE){
				toggle(c(entry.criticalall), radiob.criticalall)
				toggle(c(entry.criticalAO, entry.criticalTC, entry.criticalLS), radiob.criticalspecific)
			}
		}
		if(button==radiob.criticalspecific){
			if(button$GetActive()==TRUE){
				toggle(c(entry.criticalall), radiob.criticalall)
				toggle(c(entry.criticalAO, entry.criticalTC, entry.criticalLS), radiob.criticalspecific)
			}
		}
		if(button==checkb.outlieractive){
			toggle(c(checkb.outlierall, checkb.outlierAO, checkb.outlierTC, checkb.outlierLS), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(outlier=NULL),s)})
				status_print("outlier changed!")
			}
			else{
#				gSignalEmit(entry.outlier, "changed")
			}
		}
		if(button==checkb.outlierspanactive){
			toggle(c(entry.outlierspan1, entry.outlierspan2), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(outlier_span=NULL),s)})
				status_print("outlierspan changed!")
			}
			else{
				gSignalEmit(entry.outlierspan1, "changed")
			}
		}
		if(button==checkb.outliermethodactive){
			toggle(c(combobox.outliermethod), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(outlier_method=NULL),s)})
				status_print("outliermethod changed!")
			}
			else{
				
				gSignalEmit(combobox.outliermethod, "changed")
			}
		}
#   if(button==checkb.fileactive){
#     toggle(c(entry.file), button)
#   }
		if(button==checkb.forecast_yearsactive){
			toggle(c(entry.forecast_years), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(forecast_years=NULL),s)})
				status_print("forecast_years changed!")
			}
			else{
				gSignalEmit(entry.forecast_years, "changed")
			}
		}
		if(button==checkb.backcast_yearsactive){
			toggle(c(entry.backcast_years), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(backcast_years=NULL),s)})
				status_print("backcast_years changed!")
			}
			else{
				gSignalEmit(entry.backcast_years, "changed")
			}
		}
#   if(button==checkb.forecast_confactive){
#     toggle(c(entry.forecast_conf), button)
#     if(button$GetActive()==FALSE){
#       button$SetInconsistent(FALSE)
#       lapply(indices, FUN=function(s){x12 <<- setP(x12,list(forecast_conf=NULL),s)})
#       status_print("forecast_conf changed!")
#     }
#     else{
#       gSignalEmit(entry.forecast_conf, "changed")
#     }
#   }
		if(button==checkb.aictestactive){
			toggle(c(entry.aictest), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(aictest=NULL),s)})
				status_print("aictest changed!")
			}
			else{
				gSignalEmit(entry.aictest, "changed")
			}
		}
		if(button==checkb.seasonalmaactive){
			toggle(c(entry.seasonalma), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(seasonalma=NULL),s)})
				status_print("seasonalma changed!")
			}
			else{
				gSignalEmit(entry.seasonalma, "changed")
			}
		}
		if(button==checkb.trendmaactive){
			toggle(c(entry.trendma), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(trendma=NULL),s)})
				status_print("trendma changed!")
			}
			else{
				gSignalEmit(entry.trendma, "changed")
			}
		}
		if(button==checkb.x11calendarsigmaactive){
			toggle(c(combobox.x11calendarsigma), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(x11calendarsigma=NULL),s)})
				status_print("x11calendarsigma changed!")
			}
			else{
				gSignalEmit(combobox.x11calendarsigma, "changed")
			}
		}
		if(button==checkb.samodeactive){
			toggle(c(combobox.samode), button)
			if(button$GetActive()==FALSE){
				button$SetInconsistent(FALSE)
				lapply(indices, FUN=function(s){object <<- setP(object,list(samode=NULL),s)})
				status_print("samode changed!")
			}
			else{
				gSignalEmit(combobox.samode, "changed")
			}
		}
	}
	
	#handler for the update button
	#calls x12 and updates summaries,plots and tables
	update_handler <- function(button, userdata){
		object <<- X12(object) 
		update_notebook()
		update_outliertable()
		make_history()
	}
	
	#handler for the buttons in the manual outlier panel
	manualoutlierhandler <- function(button, userdata){
		if(button==button.manualoutlieradd){
			if(!isEmpty(entry.manualoutlieryear$GetText())&&!isEmpty(entry.manualoutlierperiod$GetText())&&
					isNumber(entry.manualoutlieryear$GetText(), integer=TRUE) &&
					isNumber(entry.manualoutlierperiod$GetText(), integer=TRUE) &&
					combobox.manualoutliertype$GetActive()!=-1){
				if(combobox.manualoutliertype$GetActive()==0) typ <- "TC"
				if(combobox.manualoutliertype$GetActive()==1) typ <- "LS"
				if(combobox.manualoutliertype$GetActive()==2) typ <- "AO"
				outlierlist[[length(outlierlist)+1]] <<- c(typ,as.numeric(entry.manualoutlieryear$GetText()),as.numeric(entry.manualoutlierperiod$GetText()))
				checkb.regvariablesactive$SetActive(TRUE);
				
				update_regvariables()
			}
		}
		
		if(button==button.manualoutlierremove){
			selection <- table.manualoutlier$GetSelection()
			if(selection$CountSelectedRows()>0){
				rows <- sapply(selection$GetSelectedRows()$retval, FUN=function(s){s$GetIndices()})+1
				outlierlist[[rows[1]]] <<- NULL
				update_regvariables()
			}
			
		}
		
		if(button == button.manualoutlieraddclick){
			if(button$GetActive()==TRUE){
				locate <<- TRUE
			}
		}
	}
	
	#updates the regvariables parameter with values from the regvariables entry and the
	#manual outlier panel
	update_regvariables <- function(){
		vars <- sapply(outlierlist, FUN=function(s){
					paste(s[1],s[2],".",s[3], sep="")
				})
		if(trim(entry.regvariables$GetText())=="" && length(vars)!=0){
			lapply(indices, FUN=function(s){object <<- setP(object,list(regvariables=as.vector(vars)),s)})
		}
		else if((trim(entry.regvariables$GetText())!="" && trim(entry.regvariables$GetText())!="*") && length(vars)==0){
			lapply(indices, FUN=function(s){object <<- setP(object,list(regvariables=cutParam(entry.regvariables$GetText())),s)})
		}
		else if((trim(entry.regvariables$GetText())=="") && length(vars)==0){
			lapply(indices, FUN=function(s){object <<- setP(object,list(regvariables=NULL),s)})
		}
		else{
			if(trim(entry.regvariables$GetText())!="*")lapply(indices, FUN=function(s){object <<- setP(object,list(regvariables=append(cutParam(entry.regvariables$GetText()), as.vector(vars))),s)})
		}
		update_outliertable()
	}
	
	#Handler for the change event of all x12parameter entrys
	#validates input, if input is valid setP is called
	x12input_handler <- function(editable, user_data){
		text <- editable$GetChars(0, 30)
		
		if(editable==entry.spanstartyear||editable==entry.spanstartperiod||editable==entry.spanendyear||editable==entry.spanendperiod){
			startyear <- entry.spanstartyear$GetChars(0, -1)
			startperiod <- entry.spanstartperiod$GetChars(0, -1)
			endyear <- entry.spanendyear$GetChars(0, -1)
			endperiod <- entry.spanendperiod$GetChars(0, -1)
			values <- c(NA,NA,NA,NA)
			problem = FALSE
			if(checkb.spanstart$GetActive()){
				if(isNumber(startyear)&&isNumber(startperiod)){
					values[1] <- as.numeric(startyear)
					values[2] <- as.numeric(startperiod)
				}
				else{
					problem = TRUE
					status_print("possible wrong input for span!")
				}
			}
			if(checkb.spanend$GetActive()){
				if(isNumber(endyear)&&isNumber(endperiod)){
					values[3] <- as.numeric(endyear)
					values[4] <- as.numeric(endperiod)
				}
				else{
					problem=TRUE
					status_print("possible wrong input for span!")
				}
			}
#     print(values)
			if(problem==FALSE){
				status_print("span changed!")
				if(length(unique(values))==1 && is.na(values[1]))lapply(indices, FUN=function(s){object <<- setP(object,list(span=NULL),s)})
				else lapply(indices, FUN=function(s){object <<- setP(object,list(span=values),s)})
			}
		}
		
		if(editable==entry.modelspanstartyear||editable==entry.modelspanstartperiod||editable==entry.modelspanendyear||editable==entry.modelspanendperiod){
			startyear <- entry.modelspanstartyear$GetChars(0, -1)
			startperiod <- entry.modelspanstartperiod$GetChars(0, -1)
			endyear <- entry.modelspanendyear$GetChars(0, -1)
			endperiod <- entry.modelspanendperiod$GetChars(0, -1)
			values <- c(NA,NA,NA,NA)
			problem = FALSE
			if(checkb.modelspanstart$GetActive()){
				if(isNumber(startyear)&&isNumber(startperiod)){
					values[1] <- as.numeric(startyear)
					values[2] <- as.numeric(startperiod)
				}
				else{
					problem = TRUE
					status_print("possible wrong input for modelspan!")
				}
			}
			if(checkb.modelspanend$GetActive()){
				if(isNumber(endyear)&&isNumber(endperiod)){
					values[3] <- as.numeric(endyear)
					values[4] <- as.numeric(endperiod)
				}
				else{
					problem=TRUE
					status_print("possible wrong input for modelspan!")
				}
			}
#     print(values)
			if(problem==FALSE){
				status_print("modelspan changed!")
				if(length(unique(values))==1 && is.na(values[1]))lapply(indices, FUN=function(s){object <<- setP(object,list(modelspan=NULL),s)})
				else lapply(indices, FUN=function(s){object <<- setP(object,list(modelspan=values),s)})
			}
		}
		
		if(editable==entry.decimals){
			if(isNumber(text)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(decimals=as.numeric(text)),s)})
				status_print("decimals changed!")
			}
			else{
				status_print("possible wrong input for decimals!")
			}
		}
		
		if(editable==entry.arima1||editable==entry.arima2||editable==entry.arima3){
			text <- entry.arima1$GetChars(0, -1)
			text2 <- entry.arima2$GetChars(0, -1)
			text3 <- entry.arima3$GetChars(0, -1)
			if(isNumber(text)&&isNumber(text2)&&isNumber(text3)){
#				print(c(as.numeric(text), as.numeric(text2), as.numeric(text3)))
				lapply(indices, FUN=function(s){object <<- setP(object,list(arima=c(as.numeric(text), as.numeric(text2), as.numeric(text3))),s)})
				status_print("arima changed!")
			}
			else{
				status_print("possible wrong input for arima!")
			}
		}
		
		if(editable==entry.sarima1||editable==entry.sarima2||editable==entry.sarima3){
			text <- entry.sarima1$GetChars(0, -1)
			text2 <- entry.sarima2$GetChars(0, -1)
			text3 <- entry.sarima3$GetChars(0, -1)
			if(isNumber(text)&&isNumber(text2)&&isNumber(text3)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(sarima=c(as.numeric(text), as.numeric(text2), as.numeric(text3))),s)})
				status_print("sarima changed!")
#				print(c(as.numeric(text), as.numeric(text2), as.numeric(text3)))
			}
			else{
				status_print("possible wrong input for sarima!")
			}
		}
		
		if(editable==entry.maxorder1||editable==entry.maxorder2){
			text <- entry.maxorder1$GetChars(0, -1)
			text2 <- entry.maxorder2$GetChars(0, -1)
			if(isNumber(text)&&isNumber(text2)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(maxorder=c(as.numeric(text), as.numeric(text2))),s)})
				status_print("maxorder changed!")
#				print(c(as.numeric(text), as.numeric(text2)))
			}
			else{
				status_print("possible wrong input for maxorder!")
			}
		}
		
		if(editable==entry.maxdiff1||editable==entry.maxdiff2){
			text <- entry.maxdiff1$GetChars(0, -1)
			text2 <- entry.maxdiff2$GetChars(0, -1)
			if(isNumber(text)&&isNumber(text2)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(maxdiff=c(as.numeric(text), as.numeric(text2))),s)})
				status_print("maxdiff changed!")
#				print(c(as.numeric(text), as.numeric(text2)))
			}
			else{
				status_print("possible wrong input for maxdiff!")
			}
		}
		
		if(editable==entry.regvariables){
			vars <- sapply(outlierlist, FUN=function(s){
						paste(s[1],s[2],".",s[3], sep="")
					})
#			print(append(cutParam(text), as.vector(vars)))
			if(trim(entry.regvariables$GetText())=="" && length(vars)!=0){
				lapply(indices, FUN=function(s){object <<- setP(object,list(regvariables=as.vector(vars)),s)})
				status_print("regvariables changed!")
			}
			else if(trim(entry.regvariables$GetText())!="" && cutParam(entry.regvariables$GetText())[1]!="*" && length(vars)==0){
				lapply(indices, FUN=function(s){object <<- setP(object,list(regvariables=cutParam(entry.regvariables$GetText())),s)})
				status_print("regvariables changed!")
			}
			else if(trim(entry.regvariables$GetText())=="" && length(vars)==0){
				lapply(indices, FUN=function(s){object <<- setP(object,list(regvariables=NULL),s)})
				status_print("regvariables changed!")
			}
			else if(cutParam(entry.regvariables$GetText())[1]!="*"){
				lapply(indices, FUN=function(s){object <<- setP(object,list(regvariables=append(cutParam(entry.regvariables$GetText()), as.vector(vars))),s)})
				status_print("regvariables changed!")
			}
#				print(cutParam(text))
		}
		
		if(editable==entry.reguser){
			if(!isEmpty(text) && trim(text) != "*"){
				lapply(indices, FUN=function(s){object <<- setP(object,list(reguser=(text)),s)})
				status_print("reguser changed!")
			}
			else{
				status_print("possible wrong input for reguser!")
			}
		}
		
		if(editable==entry.aictest){
			if(!isEmpty(text) && trim(text) != "*"){
				lapply(indices, FUN=function(s){object <<- setP(object,list(aictest=(text)),s)})
				status_print("aictest changed!")
#				print(cutParam(text))
			}
			else{
				status_print("possible wrong input for aictest!")
			}
		}
		
		if(editable==entry.usertype){
			if(grepl("^(\\s)*(seasonal|td|lpyear|user)(\\s)*((\\s)*(seasonal|td|lpyear|user)(\\s)*)*$",text)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(usertype=cutParam(text)),s)})
				status_print("usertype changed!")
#				print(cutParam(text))
			}
			else{
				status_print("possible wrong input for usertype!")
			}
		}
		
		if(editable==entry.regfilestartstartyear||editable==entry.regfilestartstartperiod){
			text <- entry.regfilestartstartyear$GetChars(0, -1)
			text2 <- entry.regfilestartstartperiod$GetChars(0, -1)
			if(isNumber(text)&&isNumber(text2)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(regfilestart=c(text, text2)),s)})
				status_print("regfilestart changed!")
#				print(c(text, text2))
			}
			else{
				status_print("possible wrong input for regfielstart!")
			}
		}
		
		if(editable==entry.seatsparameter){
			if(!isEmpty(text) && cutParam(text)!="*"){
				lapply(indices, FUN=function(s){object <<- setP(object,list(seatsparameter=cutParam(text)),s)})
				status_print("seatsparameter changed!")
#				print(cutParam(text))
			}
			else{
				status_print("possible wrong input for seatsparameter!")
			}
		}
		
		if(editable==entry.sigmalim1||editable==entry.sigmalim2){
			text <- entry.sigmalim1$GetChars(0, -1)
			text2 <- entry.sigmalim2$GetChars(0, -1)
			if(isNumber(text)&&isNumber(text2)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(sigmalim=c(as.numeric(text), as.numeric(text2))),s)})
				status_print("sigmalim changed!")
#				print(c(as.numeric(text), as.numeric(text2)))
			}
			else{
				status_print("possible wrong input for sigmalim!")
			}
		}
		
		if(editable==entry.criticalall){
			if(isNumber(text, integer=FALSE)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(critical=as.numeric(text)),s)})
				status_print("critical changed!")
#				print(cutParam(text))
			}
			else{
				status_print("possible wrong input for critical!")
			}
		}
		
		if(editable==entry.criticalAO || editable==entry.criticalLS || editable==entry.criticalTC){
			text <- entry.criticalAO$GetChars(0, -1)
			text2 <- entry.criticalLS$GetChars(0, -1)
			text3 <- entry.criticalTC$GetChars(0, -1)
			criticallist <- list()
			if(isNumber(text)){
				criticallist[["AO"]] <- as.numeric(text)
			}
			if(isNumber(text2)){
				criticallist[["LS"]] <- as.numeric(text2)
			}
			if(isNumber(text3)){
				criticallist[["TC"]] <- as.numeric(text3)
			}
			lapply(indices, FUN=function(s){object <<- setP(object,list(critical=criticallist),s)})
		}
		
#		if(editable==entry.outlier){
#			if(grepl("^(\\s)*(AO|LS|TC|all)(\\s)*((\\s)*(AO|LS|TC|all)(\\s)*)*$",text)){
#				lapply(indices, FUN=function(s){object <<- setP(object,list(outlier=cutParam(text)),s)})
#				status_print("outlier changed!")
		##				print(cutParam(text))
#			}
#			else{
#				status_print("possible wrong input for outlier!")
#			}
#		}
		
		if(editable==entry.outlierspan1||editable==entry.outlierspan2){
			text <- entry.outlierspan1$GetChars(0, -1)
			text2 <- entry.outlierspan2$GetChars(0, -1)
			if(isNumber(text)&&isNumber(text2)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(outlier_span=c(as.numeric(text), as.numeric(text2))),s)})
				status_print("outlierspan changed!")
#				print(c(as.numeric(text), as.numeric(text2)))
			}
			else{
				status_print("possible wrong input for outlierspan!")
			}
		}
		
		if(editable==entry.forecast_years){
			if(isNumber(text)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(forecast_years=as.numeric(text)),s)})
				status_print("forecast_years changed!")
			}
			else{
				status_print("possible wrong input for forecast_years!")
			}
		}
		
		if(editable==entry.backcast_years){
			if(isNumber(text)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(backcast_years=as.numeric(text)),s)})
				status_print("backcast_years changed!")
			}
			else{
				status_print("possible wrong input for backcast_years!")
			}
		}
		
		if(editable==entry.forecast_conf){
			if(isNumber(text, integer=FALSE)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(forecast_conf=as.numeric(text)),s)})
				status_print("forecast_conf changed!")
			}
			else{
				status_print("possible wrong input for forecast_conf!")
			}
		}
		
		if(editable==entry.seasonalma){
			if(isEmpty(text)==FALSE && cutParam(text)[1] != "*"){
				lapply(indices, FUN=function(s){object <<- setP(object,list(seasonalma=as.character(cutParam(text))),s)})
				status_print("seasonalma changed!")
#				print(cutParam(text))
			}
			else{
				status_print("possible wrong input for seasonalma!")
			}
		}
		
		if(editable==entry.trendma){
			if(isNumber(text)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(trendma=as.numeric(text)),s)})
				status_print("trendma changed!")
			}
			else{
				status_print("possible wrong input for trendma!")
			}
		}
		
		if(editable==entry.x11final){
			if(grepl("^(\\s)*(AO|LS|TC|user|all|none)(\\s)*((\\s)*(AO|LS|user|TC|all|none)(\\s)*)*$",text)){
				lapply(indices, FUN=function(s){object <<- setP(object,list(x11final=cutParam(text)),s)})
				status_print("x11final changed!")
#				print(cutParam(text))
			}
			else{
				status_print("possible wrong input for x11final!")
			}
		}
	}
	
	#activates the right plottingcontext(cairoDevice) and plots depending on selected tab
	#at the first run the context for each tab will be created
	update_notebook <- function(page.num=notebook.plot$GetCurrentPage(), file=FALSE, onlyplot=FALSE){
		if(page.num==0){
			if(context.plot1==0){
				asCairoDevice(area.plot1)
				context.plot1 <<- dev.cur()
			}
			if(!file)dev.set(context.plot1)
			spect <- "acf"
			if(radiob.rsdacfacf2$GetActive() == TRUE)spect <- "acf2"
			if(radiob.rsdacfpacf$GetActive() == TRUE)spect <- "pacf"
			plotRsdAcf(object@x12List[[indices[1]]], which=spect)
		}
		if(page.num==1){
			if(context.plot2==0){
				asCairoDevice(area.plot2)
				context.plot2 <<- dev.cur()
			}
			if(!file)dev.set(context.plot2)
			spect <- "sa"
			if(radiob.spectraloriginal$GetActive() == TRUE)spect <- "original"
			if(radiob.spectralresiduals$GetActive() == TRUE)spect <- "residuals"
			if(radiob.spectralirregular$GetActive() == TRUE)spect <- "irregular"
			plotSpec(object@x12List[[indices[1]]], which=spect)
		}
		if(page.num==2){
			if(context.plot3==0){
				asCairoDevice(area.plot3)
				context.plot3 <<- dev.cur()
			}
			if(!file)dev.set(context.plot3)
			plotSeasFac(object@x12List[[indices[1]]])
			
		}
		if(page.num==3){
			if(context.plot4==0){
				asCairoDevice(area.plot4)
				context.plot4 <<- dev.cur()
			}
			if(!file)dev.set(context.plot4)
			make_plot(object)
			t <- times(object@x12List[[indices[1]]])
			if(!onlyplot){
				#set values(amount of seasonal periods since begin of backcast or begin of timeseries if no backcast) for sliders
				slider.plotmin$SetRange(0, calcPeriods(t$original, object@x12List[[indices[1]]]@ts))
				slider.plotmax$SetRange(0, calcPeriods(t$original, object@x12List[[indices[1]]]@ts))
				min <- 0
				max <- calcPeriods(t$original, object@x12List[[indices[1]]]@ts)
				if(!is.null(t$forecast) && !is.null(t$backcast)){
					max <-  calcPeriods(c(t$backcast[1], t$backcast[2], t$forecast[3], t$forecast[4]), object@x12List[[indices[1]]]@ts)
				}
				if(!is.null(t$backcast) && is.null(t$forecast)){
					max <-  calcPeriods(c(t$backcast[1], t$backcast[2], t$original[3], t$original[4]), object@x12List[[indices[1]]]@ts)
				}
				if(is.null(t$backcast) && !is.null(t$forecast)){
					max <-  calcPeriods(c(t$original[1], t$original[2], t$forecast[3], t$forecast[4]), object@x12List[[indices[1]]]@ts)
				}
				slider.plotmin$SetRange(min, max)
				slider.plotmax$SetRange(min, max)
				if(!is.null(t$backcast)){
					slider.plotmin$SetValue(calcPeriods(t$backcast, object@x12List[[indices[1]]]@ts))
				}
				else{
					slider.plotmin$SetValue(0)
				}
				slider.plotmax$SetValue(calcPeriods(t$original, object@x12List[[indices[1]]]@ts))
			} 
		}
		if(page.num==4){
			if(onlyplot==FALSE)make_summary(object, table=FALSE)
		}
		if(page.num==5){
			if(onlyplot==FALSE)make_summary(object, text=FALSE)
		}
#   if(page.num==4){
#     if(context.plot5==0){
#       asCairoDevice(area.plot5)
#       context.plot5 <<- dev.cur()
#     }
#     if(!file)dev.set(context.plot5)
#     make_plot(object)
#   }
	}
	
	#Handler responsible for the contextmenus of the plots
	menuhandler <- function(menuitem, userdata){
		if(menuitem == menuitem.x12update){
			object <<- X12(object) 
			update_notebook()
			update_outliertable()
			make_history()
		}
		
		if(menuitem == menuitem.saveaspdf || menuitem == menuitem.saveaspdfwithoutlier || menuitem == menuitem.expplotaspdf){
			dialog <- gtkFileChooserDialog("Save Plot as PDF", window.main, action="save","gtk-cancel", GtkResponseType["cancel"], 
					"gtk-save", GtkResponseType["ok"])
			dialog$SetDoOverwriteConfirmation(TRUE)
			if(dialog$run()==GtkResponseType["ok"]){
				cur <- dev.cur()
				if(!grepl("(.)*(\\.pdf)",dialog$getFilename())){
					filename <- paste(dialog$getFilename(),".pdf", sep = "")
				}
				else{
					filename <- dialog$getFilename()
				}
				pdf(filename)
				update_notebook(file=TRUE)
				dev.off()
				dev.set(cur)
			}
			
			dialog$Destroy()
		}
		
		if(menuitem == menuitem.expplotaspng){
			dialog <- gtkFileChooserDialog("Save Plot as PNG", window.main, action="save","gtk-cancel", GtkResponseType["cancel"], 
					"gtk-save", GtkResponseType["ok"])
			dialog$SetDoOverwriteConfirmation(TRUE)
			if(dialog$run()==GtkResponseType["ok"]){
				cur <- dev.cur()
				if(!grepl("(.)*(\\.png)",dialog$getFilename())){
					filename <- paste(dialog$getFilename(),".png", sep = "")
				}
				else{
					filename <- dialog$getFilename()
				}
				png(filename, width=800, height=800)
				update_notebook(file=TRUE)
				dev.off()
				dev.set(cur)
			}
			
			dialog$Destroy()
		}
		
		if(menuitem == menuitem.addAO || menuitem == menuitem.addLS || menuitem == menuitem.addTC){
			oc <- convertCoordinates(clickedX)
#			offset <- floor(oc*(slider.plotmax$GetValue()-slider.plotmin$GetValue())+slider.plotmin$GetValue())
#			if(offset < 0)offset <- 0
#			t <- times(object@x12List[[indices[1]]])
#			year <- 0
#			period <- 0
#			if(!is.null(t$backcast)){
#				year <- (t$backcast[1]+floor(offset/frequency(object@x12List[[indices[1]]]@ts)))
#				period <- ((t$backcast[2]-1+offset)%%frequency(object@x12List[[indices[1]]]@ts)+1)
#			}
#			else{
#				year <- (t$original[1]+floor(offset/frequency(object@x12List[[indices[1]]]@ts)))
#				period <- ((t$original[2]-1+offset)%%frequency(object@x12List[[indices[1]]]@ts)+1)
#			}
			typ <- 0
			if(menuitem == menuitem.addAO) typ <- 'AO'
			if(menuitem == menuitem.addLS) typ <- 'LS'
			if(menuitem == menuitem.addTC) typ <- 'TC'
			
			ee <- end(object@x12List[[indices]]@ts)
			ss <- start(object@x12List[[indices]]@ts)
			if((oc$year==ee[1]&&oc$period==ee[2])||(oc$year==ss[1]&&oc$period==ss[2]))
				typ <- 'AO'
			TFexistOut <- FALSE
			if(length(outlierlist)>0){
				for(io in 1:length(outlierlist)){
					if(all(outlierlist[[io]]==c(typ,oc$year,oc$period)))
						TFexistOut <- TRUE
				}
			}
			if(!TFexistOut)
				outlierlist[[length(outlierlist)+1]] <<- c(typ,oc$year,oc$period)
			checkb.regvariablesactive$SetActive(TRUE);
			
			update_regvariables()
		}
		
		if(menuitem == menuitem.expsummarycsv || menuitem == menuitem.expsummaryclipboard){
			if(menuitem == menuitem.expsummarycsv){
				dialog <- gtkFileChooserDialog("Save CSV", window.main, action="save","gtk-cancel", GtkResponseType["cancel"], 
						"gtk-save", GtkResponseType["ok"])
				dialog$SetDoOverwriteConfirmation(TRUE)
				if(dialog$run()==GtkResponseType["ok"]){
					
					if(!grepl("(.)*(\\.csv)",dialog$getFilename())){
						filename <- paste(dialog$getFilename(),".csv", sep = "")
					}
					else{
						filename <- dialog$getFilename()
					}
				}
				nl <- max(sapply(object@x12List, function(x) length(x@x12OldOutput)))
				write.csv2(getMethod("summary","x12Batch")(object, print=FALSE, oldOutput=nl,
								fullSummary=checkb.fullSummary$GetActive(), spectra.detail=checkb.spectraldetail$GetActive(),
								almostout=checkb.almostout$GetActive(), rsd.autocorr=checkb.rsdautocorr$GetActive(),
								q2=checkb.q2$GetActive(), likelihood.stat=checkb.likelihoodstat$GetActive(),
								aape=checkb.aape$GetActive(), id.rsdseas=checkb.idrsdseas$GetActive()), file=filename, quote=FALSE)
				dialog$Destroy()
			}
			
			if(menuitem == menuitem.expsummaryclipboard){
				clipboard <- table.summary$GetClipboard(GDK_SELECTION_CLIPBOARD)
				nl <- max(sapply(object@x12List, function(x) length(x@x12OldOutput)))
				clipboard$SetText(paste(capture.output(write.csv2(getMethod("summary","x12Batch")(object, print=FALSE, oldOutput=nl,
														fullSummary=checkb.fullSummary$GetActive(), spectra.detail=checkb.spectraldetail$GetActive(),
														almostout=checkb.almostout$GetActive(), rsd.autocorr=checkb.rsdautocorr$GetActive(),
														q2=checkb.q2$GetActive(), likelihood.stat=checkb.likelihoodstat$GetActive(),
														aape=checkb.aape$GetActive(), id.rsdseas=checkb.idrsdseas$GetActive()), file="", quote=FALSE)),collapse="\n"))
			}
			
		}
		
		if(menuitem == menuitem.x12savep){
			dialog <- gtkFileChooserDialog("Save X12 Parameter", window.main, action="save","gtk-cancel", GtkResponseType["cancel"], 
					"gtk-save", GtkResponseType["ok"])
			dialog$SetDoOverwriteConfirmation(TRUE)
			if(dialog$run()==GtkResponseType["ok"]){
				
				if(!grepl("(.)*(\\.RData)",dialog$getFilename())){
					filename <- paste(dialog$getFilename(),".RData", sep = "")
				}
				else{
					filename <- dialog$getFilename()
				}
				saveP(object, filename)
			}
			
			dialog$Destroy()
		}
		if(menuitem == menuitem.x12save){
			dialog <- gtkFileChooserDialog("Save X12 Object", window.main, action="save","gtk-cancel", GtkResponseType["cancel"], 
					"gtk-save", GtkResponseType["ok"])
			dialog$SetDoOverwriteConfirmation(TRUE)
			if(dialog$run()==GtkResponseType["ok"]){
				
				if(!grepl("(.)*(\\.RData)",dialog$getFilename())){
					filename <- paste(dialog$getFilename(),".RData", sep = "")
				}
				else{
					filename <- dialog$getFilename()
				}
				save(object, file=filename)
			}
			dialog$Destroy()
		}
		
		if(menuitem == menuitem.x12load){
			dialog <- gtkFileChooserDialog("Load X12 Object", window.main, action="open","gtk-cancel", GtkResponseType["cancel"], 
					"gtk-save", GtkResponseType["ok"])
			if(dialog$run()==GtkResponseType["ok"]){
				
				if(!grepl("(.)*(\\.RData)",dialog$getFilename())){
					filename <- paste(dialog$getFilename(),".RData", sep = "")
				}
				else{
					filename <- dialog$getFilename()
				}
				gSignalHandlerBlock(table.ts$GetSelection(), handler.tstable)
				x12o <- get(load(filename))
				if(class(x12o)=="x12Single" || class(x12o)=="x12Batch"){
					if(class(x12o)=="x12Single"){
						xl <- new("x12List")
						xl <- list(x12o)
						xb <- new("x12Batch",list(x12o@ts))
						xb@x12List[[1]] <- x12o
						x12o <- X12(xb)
					}else if(class(x12o)=="ts"){
						x12o <- X12(new("x12Batch",list(x12o)))
					}	
					object <<- x12o
					elements <- sapply(object@x12List, function(x) x@tsName)
					table.ts$GetSelection()$UnselectAll()
					indices <<- c(1)
					table.model$Clear()
					sapply(elements,
							function(string) {
								iter <- table.model$Append()$iter
								table.model$Set(iter, 0, string)
							})
					setup_summarytable(object, remove=TRUE)
					read_x12(object, indices)
					update_notebook()
					update_outliertable()
					make_history()
					gSignalHandlerUnblock(table.ts$GetSelection(), handler.tstable)
				}
			}
			dialog$Destroy()
		}
		
		if(menuitem == menuitem.x12loadp){
			dialog <- gtkFileChooserDialog("Load X12 Parameter", window.main, action="open","gtk-cancel", GtkResponseType["cancel"], 
					"gtk-save", GtkResponseType["ok"])
			if(dialog$run()==GtkResponseType["ok"]){
				
				if(!grepl("(.)*(\\.RData)",dialog$getFilename())){
					filename <- paste(dialog$getFilename(),".RData", sep = "")
				}
				else{
					filename <- dialog$getFilename()
				}
				paramlist <- get(load(filename))
				dialog$Destroy()
				if(length(paramlist)>0 && class(paramlist[[1]])=="x12Parameter"){
					panel.loadplist <- gtkTable(rows=length(object@x12List)+1, columns=2)
					panel.loadplist$AttachDefaults(gtkLabel("timeseries"), 0, 1, 0, 1)
					panel.loadplist$AttachDefaults(gtkLabel("x12Parameter"), 1, 2, 0, 1)
					i <- 1
					combolist <- 1
					numparams <- length(paramlist)
					sapply(object@x12List, FUN=function(s){
								combo <- gtkComboBoxNewText()
								combo$SetSizeRequest(-1, 15)
								combo$AppendText(" ")
								sapply(seq(1,numparams,1), FUN<-function(s){combo$AppendText(s)})
								panel.loadplist$AttachDefaults(combo, 1, 2, i, i+1)
								panel.loadplist$AttachDefaults(gtkLabel(s@tsName), 0, 1, i, i+1)
								if(class(combolist)!="list")combolist <<- list(combo)
								else combolist <<- append(combolist, combo)
								i <<- i + 1
							})
					panel.scrolledload <- gtkScrolledWindow()
					panel.scrolledload$SetPolicy("GTK_POLICY_NEVER","GTK_POLICY_ALWAYS")
					panel.scrolledload$AddWithViewport(panel.loadplist)
					panel.window <- gtkVBox()
					button.accept <- gtkButton("Accept")
					button.discard <- gtkButton("Discard")
					panel.buttons <- gtkHBox()
					panel.buttons$PackStart(button.accept)
					panel.buttons$PackStart(button.discard)
					panel.window$PackStart(panel.scrolledload, expand=TRUE, fill=TRUE)
					panel.window$PackStart(panel.buttons, expand=FALSE)
					window <- gtkWindow()
					window$SetModal(TRUE)
					window$SetTitle("load Parameter")
					window$Add(panel.window)
					window$Show()
					gSignalConnect(button.accept, "released", f=function(...){
								i <- sapply(combolist, FUN<-function(s){
											if(is.null(s$GetActiveText())==FALSE)s$GetActiveText()
											else " "
										})
								for(k in 1:length(i)){
									if(isNumber(i[k]))object@x12List[[k]]@x12Parameter <<- paramlist[[as.numeric(i[k])]]
								}
								read_x12(object, indices)
								update_outliertable()
								window$Destroy()
								gtkMainQuit()
							})
					gSignalConnect(button.discard, "released", f=function(...){
								window$Destroy()
								gtkMainQuit()
							})
					gSignalConnect(window, "destroy", f=function(...){gtkMainQuit()})
					gtkMain()
				}	
			}
		}
	}
	
	filebuttonhandler <- function(widget, user_data){
#		print(filebutton.regfile$GetFile()$GetPath())
		lapply(indices, FUN=function(s){object <<- setP(object,list(regfile=filebutton.regfile$GetFile()$GetPath()),s)})
		status_print("regfile changed!")
	}
	
	#changehandler for the sliders under plot responsible for span
	sliderhandler <- function(range, user_data){
		if(range==slider.plotmin){
			if(slider.plotmin$GetValue()>slider.plotmax$GetValue()){
				slider.plotmin$SetValue(slider.plotmax$GetValue())
			}
		}
		if(range==slider.plotmax){
			if(slider.plotmax$GetValue()<slider.plotmin$GetValue()){
				slider.plotmax$SetValue(slider.plotmin$GetValue())
			}
		}
		make_plot(object)
	}
	
	#responsible for formating the seasonalperiods count of sliders to year.period formation
	sliderformat <- function(scale, value){
		t <- calcSpan(times(object@x12List[[indices[1]]]),object@x12List[[indices[1]]]@ts)
		if(scale == slider.plotmin){
			return(paste(t[1],'.',t[2]))
		}
		else if(scale == slider.plotmax){
			return(paste(t[3],'.',t[4]))
		}
	}
	
	setup_summarytable <- function(x12o, remove=FALSE){
		if(remove==TRUE){
			model.summary$Clear()
			lapply(columns.summary, FUN=function(s){table.summary$RemoveColumn(s)})
			model.summary <<- gtkListStore(rep("character", length(x12o)+3))
			table.summary$SetModel(model.summary)
		}
		i <- 0
		sumnames <- names(getMethod("summary","x12Batch")(x12o, print=FALSE))
		sumnames[1] <- "Value"
		for(s in sumnames){
			renderer <- gtkCellRendererText()
			column <- gtkTreeViewColumn()
			renderer$SetAlignment(0.5, 0.5)
			column$SetTitle(s)
			column$PackStart(renderer)
			column$SetAlignment(0.5)
			column$SetExpand(TRUE)
			column$AddAttribute(renderer, "text", i)
			if(i==0)column$AddAttribute(renderer, "background", length(sumnames))
			else column$AddAttribute(renderer, "background", length(sumnames)+1)
			i <- i + 1
			table.summary$AppendColumn(column)
			if(class(columns.summary)!="list")columns.summary <<- list(column)
			else columns.summary <<- append(columns.summary, column)
		}
	}
	
	make_summary <- function(objectS, text = TRUE, table = TRUE){
		#textform
		nl <- max(sapply(objectS@x12List, function(x) length(x@x12OldOutput)))
#		buffer.summary$SetText(paste(capture.output(getMethod("summary","x12Single")(objectS@x12List[[indices[1]]], oldOutput=nl)), collapse="\n"))
		if(text == TRUE){
			#buffer.summarytotal$SetText(paste(capture.output(getMethod("summary","x12Batch")(object, oldOutput=nl,
			#								fullSummary=checkb.fullSummary$GetActive(), spectra.detail=checkb.spectraldetail$GetActive(),
			#								almostout=checkb.almostout$GetActive(), rsd.autocorr=checkb.rsdautocorr$GetActive(),
			#								q2=checkb.q2$GetActive(), likelihood.stat=checkb.likelihoodstat$GetActive(),
			#								aape=checkb.aape$GetActive(), id.rsdseas=checkb.idrsdseas$GetActive())), collapse="\n"))
			buffer.summarytotal$SetText(paste(capture.output(getMethod("summary","x12Single")(objectS@x12List[[indices[1]]], oldOutput=nl,
											fullSummary=checkb.fullSummary$GetActive(), spectra.detail=checkb.spectraldetail$GetActive(),
											almostout=checkb.almostout$GetActive(), rsd.autocorr=checkb.rsdautocorr$GetActive(),
											q2=checkb.q2$GetActive(), likelihood.stat=checkb.likelihoodstat$GetActive(),
											aape=checkb.aape$GetActive(), id.rsdseas=checkb.idrsdseas$GetActive())), collapse="\n"))
		}
		#tableform
		if(table == TRUE){
			model.summary$Clear()
			sum <- getMethod("summary","x12Batch")(objectS, print=FALSE, oldOutput=nl,
					fullSummary=checkb.fullSummary$GetActive(), spectra.detail=checkb.spectraldetail$GetActive(),
					almostout=checkb.almostout$GetActive(), rsd.autocorr=checkb.rsdautocorr$GetActive(),
					q2=checkb.q2$GetActive(), likelihood.stat=checkb.likelihoodstat$GetActive(),
					aape=checkb.aape$GetActive(), id.rsdseas=checkb.idrsdseas$GetActive())
			iter <- model.summary$Append()$iter
			n <- 0
			nam <- names(sum)
			nam[1] <- ""
			sapply(nam, FUN=function(k){
						model.summary$Set(iter, n, k)
						n <<- n + 1
					})
			model.summary$Set(iter, n, "grey90")
			model.summary$Set(iter, n+1, "grey90")
			for(i in 1:dim(sum)[1]){
				if(grepl(".*OLD OUTPUT.*",sum[i,1])){
					iter <- model.summary$Append()$iter
					n <- 0
					sapply(sum[i,], FUN=function(k){
								model.summary$Set(iter, n, "")
								n <<- n + 1
							})
					model.summary$Set(iter, n, "white")
				}
				iter <- model.summary$Append()$iter
				n <- 0
				sapply(sum[i,], FUN=function(k){
							model.summary$Set(iter, n, k)
							n <<- n + 1
						})
				model.summary$Set(iter, n, "grey90")
				model.summary$Set(iter, n+1, "grey96")
#			model.summary$Set(iter, 0, string[1], 1, string[2], 2, string[3])
			}
		}
	}
#	setupSummarytable <- function(table, x12sum){
#		i <- 0
#		sapply(names(x12sum), FUN=function(s){
#					renderer <- gtkCellRendererText()
#					column <- gtkTreeViewColumn()
#					renderer$SetAlignment(0.5, 0.5)
#					column$SetTitle(s)
#					column$PackStart(renderer)
#					column$SetAlignment(0.5)
#					column$AddAttribute(renderer.manualoutliertype, "text", i)
#					i <<- i + 1
#					table$AppendColumn(column)
#				})
#		table
#	}
	
	#reads the x12-parameters of a x12batch from the selected indices into the gui
	#is also responsible for checking if specific aprameter is equal/different in multiple x12singles 
	#selected is a vector of the ts indices which should be used c(1,3,4) if only ts 1,3,4 should be used
	read_x12 <- function(x12batch, selected){
		v <- getP(x12batch, list("span", "modelspan", "decimals", "transform", "arima", "sarima", "automdl",
						"maxorder", "maxdiff", "regvariables", "reguser", "regfile", "automdl", "balanced", "acceptdefault", 
						"usertype", "centeruser", "regfilestart", "seats", "seatsparameter", "sigmalim", "outlier", "critical", "outlier_span", 
						"outlier_method", "forecast_years","forecast_conf", "backcast_years", "estimate", "estOutofsample",
						"slidingspans", "aictest", "onlytd", "sfshort", "samode", "seasonalma", "trendma", "x11appendfcst", "x11appendbcst",
						"x11calendarsigma", "x11excludefcst", "x11final", "x11regress"))
		v <- v[selected]
		####span
		gSignalHandlerBlock(entry.spanstartyear, handler.spanstartyear)
		gSignalHandlerBlock(entry.spanstartperiod, handler.spanstartperiod)
		gSignalHandlerBlock(entry.spanendyear, handler.spanendyear)
		gSignalHandlerBlock(entry.spanendperiod, handler.spanendperiod)
		if(length(unique(lapply(v, FUN=function(s){s$span})))==1){
			#span equal in all ts objects
			if(is.null(v[[1]]$span)){
				checkb.spanactive$SetInconsistent(FALSE)
				checkb.spanactive$SetActive(FALSE)
				update_toggle(checkb.spanactive)
			}
			else{
				checkb.spanactive$SetInconsistent(FALSE)
				checkb.spanactive$SetActive(TRUE)
				update_toggle(checkb.spanactive)
				#start
				if((is.na(v[[1]]$span)[1])||is.na((v[[1]]$span)[2])){
					checkb.spanstart$SetActive(FALSE)
					update_toggle(checkb.spanactive)
				}
				else{
					checkb.spanstart$SetActive(TRUE)
					update_toggle(checkb.spanactive)
					entry.spanstartyear$SetText(v[[1]]$span[1])
					entry.spanstartperiod$SetText(v[[1]]$span[2])
				}
				#end
				if((is.na(v[[1]]$span)[3])||is.na((v[[1]]$span)[4])){
					checkb.spanend$SetActive(FALSE)
					update_toggle(checkb.spanactive)
				}
				else{
					checkb.spanend$SetActive(TRUE)
					update_toggle(checkb.spanactive)
					entry.spanendyear$SetText(v[[1]]$span[3])
					entry.spanendperiod$SetText(v[[1]]$span[4])
				}
			}
		}
		else{
			checkb.spanactive$SetInconsistent(TRUE)
			checkb.spanactive$SetActive(TRUE)
			update_toggle(checkb.spanactive)
		}
		gSignalHandlerUnblock(entry.spanstartyear, handler.spanstartyear)
		gSignalHandlerUnblock(entry.spanstartperiod, handler.spanstartperiod)
		gSignalHandlerUnblock(entry.spanendyear, handler.spanendyear)
		gSignalHandlerUnblock(entry.spanendperiod, handler.spanendperiod)
		
		###modelspan
		gSignalHandlerBlock(entry.modelspanstartyear, handler.modelspanstartyear)
		gSignalHandlerBlock(entry.modelspanstartperiod, handler.modelspanstartperiod)
		gSignalHandlerBlock(entry.modelspanendyear, handler.modelspanendyear)
		gSignalHandlerBlock(entry.modelspanendperiod, handler.modelspanendperiod)
		if(length(unique(lapply(v, FUN=function(s){s$modelspan})))==1){
			#span equal in all ts objects
			if(is.null(v[[1]]$modelspan)){
				checkb.modelspanactive$SetInconsistent(FALSE)
				checkb.modelspanactive$SetActive(FALSE)
				update_toggle(checkb.modelspanactive)
			}
			else{
				checkb.modelspanactive$SetInconsistent(FALSE)
				checkb.modelspanactive$SetActive(TRUE)
				update_toggle(checkb.modelspanactive)
				#start
				if((is.na(v[[1]]$modelspan)[1])||is.na((v[[1]]$modelspan)[2])){
					checkb.modelspanstart$SetActive(FALSE)
					update_toggle(checkb.modelspanactive)
				}
				else{
					checkb.modelspanstart$SetActive(TRUE)
					update_toggle(checkb.modelspanactive)
					entry.modelspanstartyear$SetText(v[[1]]$modelspan[1])
					entry.modelspanstartperiod$SetText(v[[1]]$modelspan[2])
				}
				#end
				if((is.na(v[[1]]$modelspan)[3])||is.na((v[[1]]$modelspan)[4])){
					checkb.modelspanend$SetActive(FALSE)
					update_toggle(checkb.modelspanactive)
				}
				else{
					checkb.modelspanend$SetActive(TRUE)
					update_toggle(checkb.modelspanactive)
					entry.modelspanendyear$SetText(v[[1]]$modelspan[3])
					entry.modelspanendperiod$SetText(v[[1]]$modelspan[4])
				}
			}
		}
		else{
			checkb.modelspanactive$SetInconsistent(TRUE)
			checkb.modelspanactive$SetActive(TRUE)
			update_toggle(checkb.modelspanactive)
		}
		gSignalHandlerUnblock(entry.modelspanstartyear, handler.modelspanstartyear)
		gSignalHandlerUnblock(entry.modelspanstartperiod, handler.modelspanstartperiod)
		gSignalHandlerUnblock(entry.modelspanendyear, handler.modelspanendyear)
		gSignalHandlerUnblock(entry.modelspanendperiod, handler.modelspanendperiod)
		
		###decimals
		#supressing change signal while setting new values
		gSignalHandlerBlock(entry.decimals, handler.decimals)
		if(length(unique(lapply(v, FUN=function(s){s$decimals})))==1){
			if(is.null(v[[1]]$decimals)){
#       update_toggle(checkb.decimalsactive)
			}
			else{
#       update_toggle(checkb.decimalsactive)
				entry.decimals$SetText((v[[1]]$decimals))
			}
		}
		else{
			entry.decimals$SetText("*")
#     checkb.decimalsactive$SetInconsistent(TRUE)
#     checkb.decimalsactive$SetActive(TRUE)
#     update_toggle(checkb.decimalsactive)
		}
		gSignalHandlerUnblock(entry.decimals, handler.decimals)
		
		###transform
		gSignalHandlerBlock(combobox.transform, handler.transform)
		if(length(unique(lapply(v, FUN=function(s){s$transform})))==1){
			if(is.null(v[[1]]$transform)){
#       checkb.transformactive$SetActive(FALSE)
#       update_toggle(checkb.transformactive)
			}
			else{
#       checkb.transformactive$SetActive(TRUE)
#       update_toggle(checkb.transformactive)
				if((v[[1]]$transform)=="auto")combobox.transform$SetActive(0)
				if((v[[1]]$transform)=="log")combobox.transform$SetActive(1)
				if((v[[1]]$transform)=="none")combobox.transform$SetActive(2)
			}
		}
		else{
			combobox.transform$SetActive(-1)
#     checkb.transformactive$SetActive(TRUE)
#     update_toggle(checkb.transformactive)
		}
		gSignalHandlerUnblock(combobox.transform, handler.transform)
		
		###arima
		gSignalHandlerBlock(entry.arima1, handler.arima1)
		gSignalHandlerBlock(entry.arima2, handler.arima2)
		gSignalHandlerBlock(entry.arima3, handler.arima3)
		if(length(unique(lapply(v, FUN=function(s){s$arima})))==1){
			if(is.null(v[[1]]$arima)){
				checkb.arimaactive$SetInconsistent(FALSE)
				checkb.arimaactive$SetActive(FALSE)
				entry.arima1$SetText("")
				entry.arima2$SetText("")
				entry.arima3$SetText("")
				update_toggle(checkb.arimaactive)
			}
			else{
				checkb.arimaactive$SetInconsistent(FALSE)
				checkb.arimaactive$SetActive(TRUE)
				update_toggle(checkb.arimaactive)
				entry.arima1$SetText(v[[1]]$arima[1])
				entry.arima2$SetText(v[[1]]$arima[2])
				entry.arima3$SetText(v[[1]]$arima[3])
			}
		}
		else{
			entry.arima1$SetText("*")
			entry.arima2$SetText("*")
			entry.arima3$SetText("*")
			checkb.arimaactive$SetActive(TRUE)
			checkb.arimaactive$SetInconsistent(TRUE)
			update_toggle(checkb.arimaactive)
		}
		gSignalHandlerUnblock(entry.arima1, handler.arima1)
		gSignalHandlerUnblock(entry.arima2, handler.arima2)
		gSignalHandlerUnblock(entry.arima3, handler.arima3)
		
		###sarima
		gSignalHandlerBlock(entry.sarima1, handler.sarima1)
		gSignalHandlerBlock(entry.sarima2, handler.sarima2)
		gSignalHandlerBlock(entry.sarima3, handler.sarima3)
		if(length(unique(lapply(v, FUN=function(s){s$sarima})))==1){
			if(is.null(v[[1]]$sarima)){
				checkb.sarimaactive$SetInconsistent(FALSE)
				checkb.sarimaactive$SetActive(FALSE)
				entry.sarima1$SetText("")
				entry.sarima2$SetText("")
				entry.sarima3$SetText("")
				update_toggle(checkb.sarimaactive)
			}
			else{
				checkb.sarimaactive$SetInconsistent(FALSE)
				checkb.sarimaactive$SetActive(TRUE)
				update_toggle(checkb.sarimaactive)
				entry.sarima1$SetText(v[[1]]$sarima[1])
				entry.sarima2$SetText(v[[1]]$sarima[2])
				entry.sarima3$SetText(v[[1]]$sarima[3])
			}
		}
		else{
			entry.sarima1$SetText("*")
			entry.sarima2$SetText("*")
			entry.sarima3$SetText("*")
			checkb.sarimaactive$SetInconsistent(TRUE)
			checkb.sarimaactive$SetActive(TRUE)
			update_toggle(checkb.sarimaactive)
		}
		gSignalHandlerUnblock(entry.sarima1, handler.sarima1)
		gSignalHandlerUnblock(entry.sarima2, handler.sarima2)
		gSignalHandlerUnblock(entry.sarima3, handler.sarima3)
		
		###maxorder
		gSignalHandlerBlock(entry.maxorder1, handler.maxorder1)
		gSignalHandlerBlock(entry.maxorder2, handler.maxorder2)
		if(length(unique(lapply(v, FUN=function(s){s$maxorder})))==1){
			if(is.null(v[[1]]$maxorder)){
#       checkb.maxorderactive$SetActive(FALSE)
#       update_toggle(checkb.maxorderactive)
			}
			else{
#       checkb.maxorderactive$SetActive(TRUE)
#       update_toggle(checkb.maxorderactive)
				entry.maxorder1$SetText(v[[1]]$maxorder[1])
				entry.maxorder2$SetText(v[[1]]$maxorder[2])
			}
		}
		else{
			entry.maxorder1$SetText("*")
			entry.maxorder2$SetText("*")
#     checkb.maxorderactive$SetActive(TRUE)
#     update_toggle(checkb.maxorderactive)
		}
		gSignalHandlerUnblock(entry.maxorder1, handler.maxorder1)
		gSignalHandlerUnblock(entry.maxorder2, handler.maxorder2)
		
		###maxdiff
		gSignalHandlerBlock(entry.maxdiff1, handler.maxdiff1)
		gSignalHandlerBlock(entry.maxdiff2, handler.maxdiff2)
		if(length(unique(lapply(v, FUN=function(s){s$maxdiff})))==1){
			if(is.null(v[[1]]$maxdiff)){
#       checkb.maxdiffactive$SetActive(FALSE)
#       update_toggle(checkb.maxdiffactive)
			}
			else{
#       checkb.maxdiffactive$SetActive(TRUE)
#       update_toggle(checkb.maxdiffactive)
				entry.maxdiff1$SetText(v[[1]]$maxdiff[1])
				entry.maxdiff2$SetText(v[[1]]$maxdiff[2])
			}
		}
		else{
			entry.maxdiff1$SetText("*")
			entry.maxdiff2$SetText("*")
#     checkb.maxdiffactive$SetActive(TRUE)
#     update_toggle(checkb.maxdiffactive)
		}
		gSignalHandlerUnblock(entry.maxdiff1, handler.maxdiff1)
		gSignalHandlerUnblock(entry.maxdiff2, handler.maxdiff2)
		
		###regvariables
		gSignalHandlerBlock(entry.regvariables, handler.regvariables)
		if(length(unique(lapply(v, FUN=function(s){s$regvariables})))==1){
			if(is.null(v[[1]]$regvariables)){
				checkb.regvariablesactive$SetInconsistent(FALSE)
				checkb.regvariablesactive$SetActive(FALSE)
				entry.regvariables$SetText("")
				update_toggle(checkb.regvariablesactive)
				outlierlist <<- list()
			}
			else{
				checkb.regvariablesactive$SetInconsistent(FALSE)
				checkb.regvariablesactive$SetActive(TRUE)
				update_toggle(checkb.regvariablesactive)
				retval <- splitRegvariables(v[[1]]$regvariables)
#				entry.regvariables$SetText(concParam(v[[1]]$regvariables))
				entry.regvariables$SetText(concParam(retval$regvariables))
				outlierlist <<- splitOulierstring(retval$outliers)
				update_outliertable()
				
			}
		}
		else{
			entry.regvariables$SetText("*")
			outlierlist <<- list()
			checkb.sarimaactive$SetInconsistent(TRUE)
			checkb.regvariablesactive$SetActive(TRUE)
			update_toggle(checkb.regvariablesactive)
		}
		gSignalHandlerUnblock(entry.regvariables, handler.regvariables)
		
		###reguser
		gSignalHandlerBlock(entry.reguser, handler.reguser)
		if(length(unique(lapply(v, FUN=function(s){s$reguser})))==1){
			if(is.null(v[[1]]$reguser)){
				checkb.reguseractive$SetInconsistent(FALSE)
				checkb.reguseractive$SetActive(FALSE)
				entry.reguser$SetText("")
				update_toggle(checkb.reguseractive)
			}
			else{
				checkb.reguseractive$SetInconsistent(FALSE)
				checkb.reguseractive$SetActive(TRUE)
				update_toggle(checkb.reguseractive)
				entry.reguser$SetText((v[[1]]$reguser))
			}
		}
		else{
			entry.reguser$SetText("*")
			checkb.reguseractive$SetInconsistent(TRUE)
			checkb.reguseractive$SetActive(TRUE)
			update_toggle(checkb.reguseractive)
		}
		gSignalHandlerUnblock(entry.reguser, handler.reguser)
		
		###regfile
		if(length(unique(lapply(v, FUN=function(s){s$regfile})))==1){
			if(is.null(v[[1]]$regfile)){
				checkb.regfileactive$SetActive(FALSE)
				update_toggle(checkb.regfileactive)
			}
			else{
				checkb.regfileactive$SetActive(TRUE)
				update_toggle(checkb.regfileactive)
				filebutton.regfile$SetFilename((v[[1]]$regfile))
			}
		}
		else{
			filebutton.regfile$SetFilename("")
			checkb.regfileactive$SetActive(TRUE)
			update_toggle(checkb.regfileactive)
		}
		
		###usertype
		gSignalHandlerBlock(entry.usertype, handler.usertype)
		if(length(unique(lapply(v, FUN=function(s){s$usertype})))==1){
			if(is.null(v[[1]]$usertype)){
				checkb.usertypeactive$SetInconsistent(FALSE)
				checkb.usertypeactive$SetActive(FALSE)
				entry.usertype$SetText("")
				update_toggle(checkb.usertypeactive)
			}
			else{
				checkb.usertypeactive$SetInconsistent(FALSE)
				checkb.usertypeactive$SetActive(TRUE)
				update_toggle(checkb.usertypeactive)
				entry.usertype$SetText(concParam((v[[1]]$usertype)))
			}
		}
		else{
			entry.usertype$SetText("*")
			checkb.usertypeactive$SetActive(TRUE)
			checkb.usertypeactive$SetInconsistent(TRUE)
			update_toggle(checkb.usertypeactive)
		}
		gSignalHandlerUnblock(entry.usertype, handler.usertype)
		
		###centeruser
		gSignalHandlerBlock(combobox.centeruser, handler.centeruser)
		if(length(unique(lapply(v, FUN=function(s){s$centeruser})))==1){
			if(is.null(v[[1]]$centeruser)){
				checkb.centeruseractive$SetInconsistent(FALSE)
				checkb.centeruseractive$SetActive(FALSE)
				update_toggle(checkb.centeruseractive)
			}
			else{
				checkb.centeruseractive$SetInconsistent(FALSE)
				checkb.centeruseractive$SetActive(TRUE)
				update_toggle(checkb.centeruseractive)
				if((v[[1]]$centeruser)=="mean")combobox.centeruser$SetActive(0)
				if((v[[1]]$centeruser)=="seasonal")combobox.centeruser$SetActive(1)
			}
		}
		else{
			combobox.centeruser$SetActive(-1)
			checkb.centeruseractive$SetActive(TRUE)
			checkb.centeruseractive$SetInconsistent(TRUE)
			update_toggle(checkb.centeruseractive)
		}
		gSignalHandlerUnblock(combobox.centeruser, handler.centeruser)
		
		###regfilestart
		gSignalHandlerBlock(entry.regfilestartstartyear, handler.regfilestartstartyear)
		gSignalHandlerBlock(entry.regfilestartstartperiod, handler.regfilestartstartperiod)
		if(length(unique(lapply(v, FUN=function(s){s$regfilestart})))==1){
			if(is.null(v[[1]]$regfilestart)){
				checkb.regfilestartactive$SetInconsistent(FALSE)
				checkb.regfilestartactive$SetActive(FALSE)
				entry.regfilestartstartyear$SetText("")
				entry.regfilestartstartperiod$SetText("")
				update_toggle(checkb.regfilestartactive)
			}
			else{
				checkb.regfilestartactive$SetInconsistent(FALSE)
				checkb.regfilestartactive$SetActive(TRUE)
				update_toggle(checkb.regfilestartactive)
				entry.regfilestartstartyear$SetText(v[[1]]$regfilestart[1])
				entry.regfilestartstartperiod$SetText(v[[1]]$regfilestart[2])
			}
		}
		else{
			entry.regfilestartstartyear$SetText("*")
			entry.regfilestartstartperiod$SetText("*")
			checkb.regfilestartactive$SetActive(TRUE)
			checkb.regfilestartactive$SetInconsistent(TRUE)
			update_toggle(checkb.regfilestartactive)
		}
		gSignalHandlerUnblock(entry.regfilestartstartyear, handler.regfilestartstartyear)
		gSignalHandlerUnblock(entry.regfilestartstartperiod, handler.regfilestartstartperiod)
		
		###seatsparameter
		gSignalHandlerBlock(entry.seatsparameter, handler.seatsparameter)
		if(length(unique(lapply(v, FUN=function(s){s$seatsparameter})))==1){
			if(is.null(v[[1]]$seatsparameter)){
				checkb.seatsparameteractive$SetInconsistent(FALSE)
				checkb.seatsparameteractive$SetActive(FALSE)
				entry.seatsparameter$SetText("")
				update_toggle(checkb.seatsparameteractive)
			}
			else{
				checkb.seatsparameteractive$SetInconsistent(FALSE)
				checkb.seatsparameteractive$SetActive(TRUE)
				update_toggle(checkb.seatsparameteractive)
				entry.seatsparameter$SetText((v[[1]]$seatsparameter))
			}
		}
		else{
			entry.seatsparameter$SetText("*")
			checkb.seatsparameteractive$SetActive(TRUE)
			checkb.seatsparameteractive$SetInconsistent(TRUE)
			update_toggle(checkb.seatsparameteractive)
		}
		gSignalHandlerUnblock(entry.seatsparameter, handler.seatsparameter)
		
		###sigmalim
		gSignalHandlerBlock(entry.sigmalim1, handler.sigmalim1)
		gSignalHandlerBlock(entry.sigmalim2, handler.sigmalim2)
		if(length(unique(lapply(v, FUN=function(s){s$sigmalim})))==1){
			if(is.null(v[[1]]$sigmalim)){
				checkb.sigmalimactive$SetInconsistent(FALSE)
				checkb.sigmalimactive$SetActive(FALSE)
				entry.sigmalim1$SetText("")
				entry.sigmalim2$SetText("")
				update_toggle(checkb.sigmalimactive)
			}
			else{
				checkb.sigmalimactive$SetInconsistent(FALSE)
				checkb.sigmalimactive$SetActive(TRUE)
				update_toggle(checkb.sigmalimactive)
				entry.sigmalim1$SetText(v[[1]]$sigmalim[1])
				entry.sigmalim2$SetText(v[[1]]$sigmalim[2])
			}
		}
		else{
			entry.sigmalim1$SetText("*")
			entry.sigmalim2$SetText("*")
			checkb.sigmalimactive$SetActive(TRUE)
			checkb.sigmalimactive$SetInconsistent(TRUE)
			update_toggle(checkb.sigmalimactive)
		}
		gSignalHandlerUnblock(entry.sigmalim1, handler.sigmalim1)
		gSignalHandlerUnblock(entry.sigmalim2, handler.sigmalim2)
		
		###critical
		gSignalHandlerBlock(entry.criticalall, handler.criticalall)
		gSignalHandlerBlock(entry.criticalTC, handler.criticalTC)
		gSignalHandlerBlock(entry.criticalLS, handler.criticalLS)
		gSignalHandlerBlock(entry.criticalAO, handler.criticalAO)
		if(length(unique(lapply(v, FUN=function(s){s$critical})))==1){
			if(is.null(v[[1]]$critical)){
				checkb.criticalactive$SetInconsistent(FALSE)
				checkb.criticalactive$SetActive(FALSE)
				entry.criticalAO$SetText("")
				entry.criticalLS$SetText("")
				entry.criticalTC$SetText("")
				entry.criticalall$SetText("")
				update_toggle(checkb.criticalactive)
			}
			else{
				checkb.criticalactive$SetInconsistent(FALSE)
				checkb.criticalactive$SetActive(TRUE)
				update_toggle(checkb.criticalactive)
				if(length(v[[1]]$critical)==1 && is.null(names(v[[1]]$critical))){
					entry.criticalall$SetText(v[[1]]$critical[1])
					radiob.criticalall$SetActive(TRUE)
					radiob.criticalspecific$SetActive(FALSE)
				}
				else{
					radiob.criticalall$SetActive(FALSE)
					radiob.criticalspecific$SetActive(TRUE)
					if(is.null(v[[1]]$critical[["TC"]])==FALSE)entry.criticalTC$SetText(v[[1]]$critical["TC"])
					else entry.criticalTC$SetText(" ")
					if(is.null(v[[1]]$critical[["LS"]])==FALSE)entry.criticalLS$SetText(v[[1]]$critical["LS"])
					else entry.criticalLS$SetText(" ")
					if(is.null(v[[1]]$critical[["AO"]])==FALSE)entry.criticalAO$SetText(v[[1]]$critical["AO"])
					else entry.criticalAO$SetText(" ")
				}
			}
		}
		else{
			entry.criticalAO$SetText("*")
			entry.criticalLS$SetText("*")
			entry.criticalTC$SetText("*")
			entry.criticalall$SetText("*")
			checkb.criticalactive$SetActive(TRUE)
			checkb.criticalactive$SetInconsistent(TRUE)
			update_toggle(checkb.criticalactive)
		}
		gSignalHandlerUnblock(entry.criticalall, handler.criticalall)
		gSignalHandlerUnblock(entry.criticalTC, handler.criticalTC)
		gSignalHandlerUnblock(entry.criticalLS, handler.criticalLS)
		gSignalHandlerUnblock(entry.criticalAO, handler.criticalAO)
		
		###outlier
#		gSignalHandlerBlock(entry.outlier, handler.outlier)
		gSignalHandlerBlock(checkb.outlierall, handler.outlierall)
		gSignalHandlerBlock(checkb.outlierTC, handler.outlierTC)
		gSignalHandlerBlock(checkb.outlierAO, handler.outlierAO)
		gSignalHandlerBlock(checkb.outlierLS, handler.outlierLS)
		if(length(unique(lapply(v, FUN=function(s){s$outlier})))==1){
			if(is.null(v[[1]]$outlier)){
				checkb.outlieractive$SetInconsistent(FALSE)
				checkb.outlieractive$SetActive(FALSE)
#				entry.outlier$SetText("")
				update_toggle(checkb.outlieractive)
			}
			else{
				checkb.outlieractive$SetInconsistent(FALSE)
				checkb.outlierall$SetInconsistent(FALSE)
				checkb.outlierTC$SetInconsistent(FALSE)
				checkb.outlierAO$SetInconsistent(FALSE)
				checkb.outlierLS$SetInconsistent(FALSE)
				checkb.outlieractive$SetActive(TRUE)
				update_toggle(checkb.outlieractive)
#				entry.outlier$SetText(concParam(v[[1]]$outlier))
				checkb.outlierall$SetActive(FALSE)
				checkb.outlierTC$SetActive(FALSE)
				checkb.outlierAO$SetActive(FALSE)
				checkb.outlierLS$SetActive(FALSE)
				for(i in v[[1]]$outlier){
					if(i=="all")checkb.outlierall$SetActive(TRUE)
					if(i=="TC")checkb.outlierTC$SetActive(TRUE)
					if(i=="AO")checkb.outlierAO$SetActive(TRUE)
					if(i=="LS")checkb.outlierLS$SetActive(TRUE)
				}
				toggle(c(checkb.outlierTC, checkb.outlierAO, checkb.outlierLS), checkb.outlierall, invert=TRUE)
			}
		}
		else{
			#entry.outlier$SetText("*")
			checkb.outlierall$SetInconsistent(TRUE)
			checkb.outlierTC$SetInconsistent(TRUE)
			checkb.outlierAO$SetInconsistent(TRUE)
			checkb.outlierLS$SetInconsistent(TRUE)
			checkb.outlieractive$SetActive(TRUE)
			checkb.outlieractive$SetInconsistent(TRUE)
			update_toggle(checkb.outlieractive)
		}
		gSignalHandlerUnblock(checkb.outlierall, handler.outlierall)
		gSignalHandlerUnblock(checkb.outlierTC, handler.outlierTC)
		gSignalHandlerUnblock(checkb.outlierAO, handler.outlierAO)
		gSignalHandlerUnblock(checkb.outlierLS, handler.outlierLS)
#		gSignalHandlerUnblock(entry.outlier, handler.outlier)
		
		###outlierspan
		gSignalHandlerBlock(entry.outlierspan1, handler.outlierspan1)
		gSignalHandlerBlock(entry.outlierspan2, handler.outlierspan2)
		if(length(unique(lapply(v, FUN=function(s){s$outlier_span})))==1){
			if(is.null(v[[1]]$outlier_span)){
				checkb.outlierspanactive$SetInconsistent(FALSE)
				checkb.outlierspanactive$SetActive(FALSE)
				entry.outlierspan1$SetText("")
				entry.outlierspan2$SetText("")
				update_toggle(checkb.outlierspanactive)
			}
			else{
				checkb.outlierspanactive$SetInconsistent(FALSE)
				checkb.outlierspanactive$SetActive(TRUE)
				update_toggle(checkb.outlierspanactive)
				entry.outlierspan1$SetText(v[[1]]$outlier_span[1])
				entry.outlierspan2$SetText(v[[1]]$outlier_span[2])
			}
		}
		else{
			entry.outlierspan1$SetText("*")
			entry.outlierspan2$SetText("*")
			checkb.outlierspanactive$SetActive(TRUE)
			checkb.outlierspanactive$SetInconsistent(TRUE)
			update_toggle(checkb.outlierspanactive)
		}
		gSignalHandlerUnblock(entry.outlierspan1, handler.outlierspan1)
		gSignalHandlerUnblock(entry.outlierspan2, handler.outlierspan2)
		
		###outliermethod
		gSignalHandlerBlock(combobox.outliermethod, handler.outliermethod)
		if(length(unique(lapply(v, FUN=function(s){s$outliermethod})))==1){
			if(is.null(v[[1]]$outliermethod)){
				checkb.outliermethodactive$SetInconsistent(FALSE)
				checkb.outliermethodactive$SetActive(FALSE)
				update_toggle(checkb.outliermethodactive)
			}
			else{
				checkb.outliermethodactive$SetInconsistent(FALSE)
				checkb.outliermethodactive$SetActive(TRUE)
				update_toggle(checkb.outliermethodactive)
				if((v[[1]]$outliermethod)=="addone")combobox.outliermethod$SetActive(0)
				if((v[[1]]$outliermethod)=="addall")combobox.outliermethod$SetActive(1)
			}
		}
		else{
			combobox.outliermethod$SetActive(-1)
			checkb.outliermethodactive$SetActive(TRUE)
			checkb.outliermethodactive$SetInconsistent(TRUE)
			update_toggle(checkb.outliermethodactive)
		}
		gSignalHandlerUnblock(combobox.outliermethod, handler.outliermethod)
		
		###samode
		gSignalHandlerBlock(combobox.samode, handler.samode)
		if(length(unique(lapply(v, FUN=function(s){s$samode})))==1){
			if(is.null(v[[1]]$samode)){
				checkb.samodeactive$SetInconsistent(FALSE)
				checkb.samodeactive$SetActive(FALSE)
				update_toggle(checkb.samodeactive)
			}
			else{
				checkb.samodeactive$SetInconsistent(FALSE)
				checkb.samodeactive$SetActive(TRUE)
				update_toggle(checkb.samodeactive)
				if((v[[1]]$samode)=="mult")combobox.samode$SetActive(0)
				if((v[[1]]$samode)=="add")combobox.samode$SetActive(1)
				if((v[[1]]$samode)=="pseudoadd")combobox.samode$SetActive(2)
				if((v[[1]]$samode)=="logadd")combobox.samode$SetActive(3)
			}
		}
		else{
			combobox.samode$SetActive(-1)
			checkb.samodeactive$SetActive(TRUE)
			checkb.samodeactive$SetInconsistent(TRUE)
			update_toggle(checkb.samodeactive)
		}
		gSignalHandlerUnblock(combobox.samode, handler.samode)
		
		###forecast_years
		gSignalHandlerBlock(entry.forecast_years, handler.forecast_years)
		if(length(unique(lapply(v, FUN=function(s){s$forecast_years})))==1){
			if(is.null(v[[1]]$forecast_years)){
				checkb.forecast_yearsactive$SetInconsistent(FALSE)
				checkb.forecast_yearsactive$SetActive(FALSE)
				entry.forecast_years$SetText("")
				update_toggle(checkb.forecast_yearsactive)
			}
			else{
				checkb.forecast_yearsactive$SetInconsistent(FALSE)
				checkb.forecast_yearsactive$SetActive(TRUE)
				update_toggle(checkb.forecast_yearsactive)
				entry.forecast_years$SetText((v[[1]]$forecast_years))
			}
		}
		else{
			entry.forecast_years$SetText("*")
			checkb.forecast_yearsactive$SetActive(TRUE)
			checkb.forecast_yearsactive$SetInconsistent(TRUE)
			update_toggle(checkb.forecast_yearsactive)
		}
		gSignalHandlerUnblock(entry.forecast_years, handler.forecast_years)
		
		###forecast_conf
		gSignalHandlerBlock(entry.forecast_conf, handler.forecast_conf)
		if(length(unique(lapply(v, FUN=function(s){s$forecast_conf})))==1){
			if(is.null(v[[1]]$forecast_conf)){
#       checkb.forecast_confactive$SetActive(FALSE)
#       update_toggle(checkb.forecast_confactive)
			}
			else{
#       checkb.forecast_confactive$SetActive(TRUE)
#       update_toggle(checkb.forecast_confactive)
				entry.forecast_conf$SetText((v[[1]]$forecast_conf))
			}
		}
		else{
			entry.forecast_conf$SetText("*")
#     checkb.forecast_confactive$SetActive(TRUE)
#     update_toggle(checkb.forecast_confactive)
		}
		gSignalHandlerUnblock(entry.forecast_conf, handler.forecast_conf)
		
		###backcast_years
		gSignalHandlerBlock(entry.backcast_years, handler.backcast_years)
		if(length(unique(lapply(v, FUN=function(s){s$backcast_years})))==1){
			if(is.null(v[[1]]$backcast_years)){
				checkb.backcast_yearsactive$SetInconsistent(FALSE)
				checkb.backcast_yearsactive$SetActive(FALSE)
				entry.backcast_years$SetText("")
				update_toggle(checkb.backcast_yearsactive)
			}
			else{
				checkb.backcast_yearsactive$SetInconsistent(FALSE)
				checkb.backcast_yearsactive$SetActive(TRUE)
				update_toggle(checkb.backcast_yearsactive)
				entry.backcast_years$SetText((v[[1]]$backcast_years))
			}
		}
		else{
			entry.backcast_years$SetText("*")
			checkb.backcast_yearsactive$SetActive(TRUE)
			checkb.backcast_yearsactive$SetInconsistent(TRUE)
			update_toggle(checkb.backcast_yearsactive)
		}
		gSignalHandlerUnblock(entry.backcast_years, handler.backcast_years)
		
		###aictest
		gSignalHandlerBlock(entry.aictest, handler.aictest)
		if(length(unique(lapply(v, FUN=function(s){s$aictest})))==1){
			if(is.null(v[[1]]$aictest)){
				checkb.aictestactive$SetInconsistent(FALSE)
				checkb.aictestactive$SetActive(FALSE)
				entry.aictest$SetText("")
				update_toggle(checkb.aictestactive)
			}
			else{
				checkb.aictestactive$SetInconsistent(FALSE)
				checkb.aictestactive$SetActive(TRUE)
				update_toggle(checkb.aictestactive)
				entry.aictest$SetText(concParam((v[[1]]$aictest)))
			}
		}
		else{
			entry.aictest$SetText("*")
			checkb.aictestactive$SetActive(TRUE)
			checkb.aictestactive$SetInconsistent(TRUE)
			update_toggle(checkb.aictestactive)
		}
		gSignalHandlerUnblock(entry.aictest, handler.aictest)
		
		###seasonalma
		gSignalHandlerBlock(entry.seasonalma, handler.seasonalma)
		if(length(unique(lapply(v, FUN=function(s){s$seasonalma})))==1){
			if(is.null(v[[1]]$seasonalma)){
				checkb.seasonalmaactive$SetInconsistent(FALSE)
				checkb.seasonalmaactive$SetActive(FALSE)
				entry.seasonalma$SetText("")
				update_toggle(checkb.seasonalmaactive)
			}
			else{
				checkb.seasonalmaactive$SetInconsistent(FALSE)
				checkb.seasonalmaactive$SetActive(TRUE)
				update_toggle(checkb.seasonalmaactive)
				entry.seasonalma$SetText(concParam((v[[1]]$seasonalma)))
			}
		}
		else{
			entry.seasonalma$SetText("*")
			checkb.seasonalmaactive$SetActive(TRUE)
			checkb.seasonalmaactive$SetInconsistent(TRUE)
			update_toggle(checkb.seasonalmaactive)
		}
		gSignalHandlerUnblock(entry.seasonalma, handler.seasonalma)
		
		###trendma
		gSignalHandlerBlock(entry.trendma, handler.trendma)
		if(length(unique(lapply(v, FUN=function(s){s$trendma})))==1){
			if(is.null(v[[1]]$trendma)){
				checkb.trendmaactive$SetInconsistent(FALSE)
				checkb.trendmaactive$SetActive(FALSE)
				entry.trendma$SetText("")
				update_toggle(checkb.trendmaactive)
			}
			else{
				checkb.trendmaactive$SetInconsistent(FALSE)
				checkb.trendmaactive$SetActive(TRUE)
				update_toggle(checkb.trendmaactive)
				entry.trendma$SetText((v[[1]]$trendma))
			}
		}
		else{
			entry.trendma$SetText("*")
			checkb.trendmaactive$SetActive(TRUE)
			checkb.trendmaactive$SetInconsistent(TRUE)
			update_toggle(checkb.trendmaactive)
		}
		gSignalHandlerUnblock(entry.trendma, handler.trendma)
		
		
		###x11calendarsigma
		gSignalHandlerBlock(combobox.x11calendarsigma, handler.x11calendarsigma)
		if(length(unique(lapply(v, FUN=function(s){s$x11calendarsigma})))==1){
			if(is.null(v[[1]]$x11calendarsigma)){
				checkb.x11calendarsigmaactive$SetActive(FALSE)
				update_toggle(checkb.x11calendarsigmaactive)
			}
			else{
				checkb.x11calendarsigmaactive$SetActive(TRUE)
				update_toggle(checkb.x11calendarsigmaactive)
				if((v[[1]]$x11calendarsigma)=="all")combobox.x11calendarsigma$SetActive(0)
				if((v[[1]]$x11calendarsigma)=="signif")combobox.x11calendarsigma$SetActive(1)
				if((v[[1]]$x11calendarsigma)=="select")combobox.x11calendarsigma$SetActive(2)
			}
		}
		else{
			combobox.x11calendarsigma$SetActive(-1)
			checkb.x11calendarsigmaactive$SetActive(TRUE)
			update_toggle(checkb.x11calendarsigmaactive)
		}
		gSignalHandlerUnblock(combobox.x11calendarsigma, handler.x11calendarsigma)
		
		###x11final
		gSignalHandlerBlock(entry.x11final, handler.x11final)
		if(length(unique(lapply(v, FUN=function(s){s$x11final})))==1){
			if(is.null(v[[1]]$x11final)){
#       checkb.x11finalactive$SetInconsistent(FALSE)
#       checkb.x11finalactive$SetActive(FALSE)
#       update_toggle(checkb.x11finalactive)
			}
			else{
#       checkb.x11finalactive$SetInconsistent(FALSE)
#       checkb.x11finalactive$SetActive(TRUE)
#       update_toggle(checkb.x11finalactive)
				entry.x11final$SetText(concParam((v[[1]]$x11final)))
			}
		}
		else{
			entry.x11final$SetText("*")
#     checkb.x11finalactive$SetActive(TRUE)
#     checkb.x11finalactive$SetInconsistent(TRUE)
#     update_toggle(checkb.x11finalactive)
		}
		gSignalHandlerUnblock(entry.x11final, handler.x11final)
		
		###automdl
		gSignalHandlerBlock(checkb.automdl, handlercheckb.automdl)
		if(length(unique(lapply(v, FUN=function(s){s$automdl})))==1){
			checkb.automdl$SetInconsistent(FALSE)
			if(v[[1]]$automdl==TRUE)checkb.automdl$SetActive(TRUE)
			else checkb.automdl$SetActive(FALSE)
		}
		else{
			checkb.automdl$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.automdl, handlercheckb.automdl)
		
		###balanced
		gSignalHandlerBlock(checkb.balanced, handlercheckb.balanced)
		if(length(unique(lapply(v, FUN=function(s){s$balanced})))==1){
			checkb.balanced$SetInconsistent(FALSE)
			if(v[[1]]$balanced==TRUE)checkb.balanced$SetActive(TRUE)
			else checkb.balanced$SetActive(FALSE)
		}
		else{
			checkb.balanced$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.balanced, handlercheckb.balanced)
		
		###acceptdefault
		gSignalHandlerBlock(checkb.acceptdefault, handlercheckb.acceptdefault)
		if(length(unique(lapply(v, FUN=function(s){s$acceptdefault})))==1){
			checkb.acceptdefault$SetInconsistent(FALSE)
			if(v[[1]]$acceptdefault==TRUE)checkb.acceptdefault$SetActive(TRUE)
			else checkb.acceptdefault$SetActive(FALSE)
		}
		else{
			checkb.acceptdefault$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.acceptdefault, handlercheckb.acceptdefault)
		
		###seats
		gSignalHandlerBlock(checkb.seats, handlercheckb.seats)
		if(length(unique(lapply(v, FUN=function(s){s$seats})))==1){
			checkb.seats$SetInconsistent(FALSE)
			if(v[[1]]$seats==TRUE)checkb.seats$SetActive(TRUE)
			else checkb.seats$SetActive(FALSE)
		}
		else{
			checkb.seats$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.seats, handlercheckb.seats)
		
		###estimate
		gSignalHandlerBlock(checkb.estimate, handlercheckb.estimate)
		if(length(unique(lapply(v, FUN=function(s){s$estimate})))==1){
			checkb.estimate$SetInconsistent(FALSE)
			if(v[[1]]$estimate==TRUE)checkb.estimate$SetActive(TRUE)
			else checkb.estimate$SetActive(FALSE)
		}
		else{
			checkb.estimate$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.estimate, handlercheckb.estimate)
		
		###estOutofsample
		gSignalHandlerBlock(checkb.estOutofsample, handlercheckb.estOutofsample)
		if(length(unique(lapply(v, FUN=function(s){s$estOutofsample})))==1){
			checkb.estOutofsample$SetInconsistent(FALSE)
			if(v[[1]]$estOutofsample==TRUE)checkb.estOutofsample$SetActive(TRUE)
			else checkb.estOutofsample$SetActive(FALSE)
		}
		else{
			checkb.estOutofsample$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.estOutofsample, handlercheckb.estOutofsample)
		
		###slidingspans
		gSignalHandlerBlock(checkb.slidingspans, handlercheckb.slidingspans)
		if(length(unique(lapply(v, FUN=function(s){s$slidingspans})))==1){
			checkb.slidingspans$SetInconsistent(FALSE)
			if(v[[1]]$slidingspans==TRUE)checkb.slidingspans$SetActive(TRUE)
			else checkb.slidingspans$SetActive(FALSE)
		}
		else{
			checkb.slidingspans$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.slidingspans, handlercheckb.slidingspans)
		
		###onlytd
		gSignalHandlerBlock(checkb.onlytd, handlercheckb.onlytd)
		if(length(unique(lapply(v, FUN=function(s){s$onlytd})))==1){
			checkb.onlytd$SetInconsistent(FALSE)
			if(v[[1]]$onlytd==TRUE)checkb.onlytd$SetActive(TRUE)
			else checkb.onlytd$SetActive(FALSE)
		}
		else{
			checkb.onlytd$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.onlytd, handlercheckb.onlytd)
		
		###sfshort
		gSignalHandlerBlock(checkb.sfshort, handlercheckb.sfshort)
		if(length(unique(lapply(v, FUN=function(s){s$sfshort})))==1){
			checkb.sfshort$SetInconsistent(FALSE)
			if(v[[1]]$sfshort==TRUE)checkb.sfshort$SetActive(TRUE)
			else checkb.sfshort$SetActive(FALSE)
		}
		else{
			checkb.sfshort$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.sfshort, handlercheckb.sfshort)
		
		###x11appendfcst
		gSignalHandlerBlock(checkb.x11appendfcst, handlercheckb.x11appendfcst)
		if(length(unique(lapply(v, FUN=function(s){s$x11appendfcst})))==1){
			checkb.x11appendfcst$SetInconsistent(FALSE)
			if(v[[1]]$x11appendfcst==TRUE)checkb.x11appendfcst$SetActive(TRUE)
			else checkb.x11appendfcst$SetActive(FALSE)
		}
		else{
			checkb.x11appendfcst$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.x11appendfcst, handlercheckb.x11appendfcst)
		
		###x11appendbcst
		gSignalHandlerBlock(checkb.x11appendbcst, handlercheckb.x11appendbcst)
		if(length(unique(lapply(v, FUN=function(s){s$x11appendbcst})))==1){
			checkb.x11appendbcst$SetInconsistent(FALSE)
			if(v[[1]]$x11appendbcst==TRUE)checkb.x11appendbcst$SetActive(TRUE)
			else checkb.x11appendbcst$SetActive(FALSE)
		}
		else{
			checkb.x11appendbcst$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.x11appendbcst, handlercheckb.x11appendbcst)
		
		###x11excludefcst
		gSignalHandlerBlock(checkb.x11excludefcst, handlercheckb.x11excludefcst)
		if(length(unique(lapply(v, FUN=function(s){s$x11excludefcst})))==1){
			checkb.x11excludefcst$SetInconsistent(FALSE)
			if(v[[1]]$x11excludefcst==TRUE)checkb.x11excludefcst$SetActive(TRUE)
			else checkb.x11excludefcst$SetActive(FALSE)
		}
		else{
			checkb.x11excludefcst$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.x11excludefcst, handlercheckb.x11excludefcst)
		
		###x11regress
		gSignalHandlerBlock(checkb.x11regress, handlercheckb.x11regress)
		if(length(unique(lapply(v, FUN=function(s){s$x11regress})))==1){
			checkb.x11regress$SetInconsistent(FALSE)
			if(v[[1]]$x11regress==TRUE)checkb.x11regress$SetActive(TRUE)
			else checkb.x11regress$SetActive(FALSE)
		}
		else{
			checkb.x11regress$SetInconsistent(TRUE)
		}
		gSignalHandlerUnblock(checkb.x11regress, handlercheckb.x11regress)
		
		make_history()
	}
	
	#updates the history combobox to values correspondig the old outputs
	make_history <- function(){
		###History Combobox
		for(i in 1:(count.history+1))combobox.history$RemoveText(0)
		k <- length(object@x12List[[indices[1]]]@x12OldParameter)
		if(k>0){
			combobox.history$AppendText("previous")
			count.history <<- 0
			for(i in k:1){
				combobox.history$AppendText(i)
				count.history <<- count.history + 1
			}
			combobox.history$SetActive(0)
		}
	}
	
# make_plotFbcast <- function(...){
#   s <- capture.output(plotFbcast(x12@x12List[[indices[1]]], backcast=checkb.backcast$GetActive(), forecast=checkb.forecast$GetActive(),
#       showCI=checkb.showCI$GetActive(), log_transform=checkb.logtransform_fb$GetActive(), 
#       showLine=checkb.showLine$GetActive(), 
#       points_original=checkb.pointsOriginal$GetActive()))
#   if(is.character(s) & length(s)>0){
#     status_print(s)
#   }
# }
	
	#draws plot depending on parameters set in gui
	make_plot <- function(objectP,...){
#		print(times(x12@x12List[[indices[1]]]))
#		print(calcSpan(times(x12@x12List[[indices[1]]]),x12@x12List[[indices[1]]]@ts))
		showallout = FALSE;
		capture.output(v <- getP(objectP, list("regvariables")))
		v <- v[indices]
		showout <- NULL
		if(checkb.showout$GetActive() == TRUE){
			if(isNumber(entry.showoutyear$GetText()) == TRUE && isNumber(entry.showoutperiod$GetText()) == TRUE &&
					as.numeric(entry.showoutperiod$GetText()) <= frequency(objectP@x12List[[indices[1]]]@ts) &&
					as.numeric(entry.showoutyear$GetText()) >= start(objectP@x12List[[indices[1]]]@ts) &&
					as.numeric(entry.showoutyear$GetText()) <= end(objectP@x12List[[indices[1]]]@ts)){
				showout <- paste(entry.showoutyear$GetText(),".",entry.showoutperiod$GetText(),sep="")
			}
		}
#		if(length(v[[1]])>0 && checkb.showAllout$GetActive()==TRUE)showallout=TRUE;
		s <- capture.output(plotgui(objectP@x12List[[indices[1]]]@x12Output, sa=checkb.sa$GetActive(), trend=checkb.trend$GetActive(), 
						log_transform=checkb.logtransform$GetActive(),
						showCI=checkb.showCI$GetActive(), 
						points_original=checkb.pointsOriginal$GetActive(),showAllout=checkb.showAllout$GetActive(),
						span=calcSpan(times(objectP@x12List[[indices[1]]]),objectP@x12List[[indices[1]]]@ts),showOut=showout))
		if(is.character(s) & length(s)>0){
			status_print(s)
		}
	}
	
	#checks per regular expression if string contains a integer
	#used to make code more readable
	isNumber <- function(s, integer=TRUE){
		if(integer==TRUE){
			grepl("^(\\s)*[0-9]+(\\s)*$",s)
		}
		else {
			grepl("^(\\s)*[0-9]+(\\.[0-9]+)?(\\s)*$",s)
		}
	}
	
	#checks if list contains NULL-element
	containsNULL <- function(s){
		nulls <- sapply(s, FUN=function(s){is.null(s)})
		if(length(unique(nulls))==2||nulls[1]==TRUE)return(TRUE)
		else return(FALSE)
	}
	
	#calculates the amount of seasonalperiods of a specific timeseries in 
	#interval c(startyear, startperiod, endyear, endperiod)
	calcPeriods <- function(interval, tss){
		f <- frequency(tss)
		points <- (interval[3]-(interval[1]+1))*f + (f-interval[2]) + interval[4]
		return(points)
	}
	
	#calculates a vector of form c(startyear, startperiod, endyear, endperiod) from
	#the timeselement of the x12Object, a timeseries(for the frequency) and the values of the sliders
	#in the gui
	calcSpan <- function(t, tss){
#		startdate <- c(floor(slider.plotmin$getValue()/(frequency(tss)+1)), slider.plotmin$getValue()%%(frequency(tss)+1))
#		enddate <- c(floor(slider.plotmax$getValue()/(frequency(tss)+1)), slider.plotmax$getValue()%%(frequency(tss)+1))
#		print(enddate)
#		if(!is.null(t$backcast)){
#			return(c((t$backcast[1]+startdate[1])+floor(((t$backcast[2]+startdate[2])/(frequency(tss)+1))),
#							((t$backcast[2]+startdate[2])%%(frequency(tss)+1)),
#							(t$backcast[1]+enddate[1])+floor(((t$backcast[2]+enddate[2])/(frequency(tss)+1))),
#							((t$backcast[2]+enddate[2])%%(frequency(tss)+1))+1))
#		}
#		else{
#			return(c((t$original[1]+startdate[1])+floor(((t$original[2]+startdate[2])/(frequency(tss)+1))),
#							((t$original[2]+startdate[2])%%(frequency(tss)+1)+1),
#							(t$original[1]+enddate[1])+floor(((t$original[2]+enddate[2])/(frequency(tss)+1))),
#							((t$original[2]+enddate[2])%%(frequency(tss)+1))+1))
#		}
		min <- slider.plotmin$getValue()
		max <- slider.plotmax$getValue()
		if(!is.null(t$backcast)){
			return(c((t$backcast[1]+floor(min/frequency(tss))),
							((t$backcast[2]-1+min)%%frequency(tss)+1),
							(t$backcast[1]+floor(max/frequency(tss))),
							((t$backcast[2]-1+max)%%frequency(tss)+1)))
		}
		else{
			return(c((t$original[1]+floor(min/frequency(tss))),
							((t$original[2]-1+min)%%frequency(tss)+1),
							(t$original[1]+floor(max/frequency(tss))),
							((t$original[2]-1+max)%%frequency(tss)+1)))
		}
	}
	
	#splits the regvariables from getP() into two seperate lists, one containing the regvariables for the regvariables entry
	#the other the manual outliners for the corresponding gui
	splitRegvariables <- function(rv){
		outliers <- vector()
		regvariables <- vector()
		
		for(s in rv){
			if(grepl("^(\\s)*(AO|LS|TC)[0-9]*\\.[0-9]*(\\s)*$",s)==TRUE){
				outliers <- append(outliers, trim(s))
			}
			else{
				regvariables <- append(regvariables, trim(s))
			}
		}	
		return(list(outliers=outliers, regvariables=regvariables))
	}
	
	#splits a list like list("AO1950.5","LS94234.34") into list(c("AO",1950,5),c("LS",94234,34))
	splitOulierstring <- function(os){
		lapply(os, FUN=function(s){
					k <- strsplit(s,"\\.")
					return(c(substr(k[[1]][1],1,2),gsub("(AO|LS|TC)","",k[[1]][1]),k[[1]][2]))
				})
	}
	
	#checks per regular expression if string is empty(only consists of whitespaces)
	#used to make code more readable
	isEmpty <- function(s){
		grepl("^(\\s)*$",s)
	}
	
	#splits a string of form (param1 param2 param3) into a vector(c(param1,param2,param3))
	cutParam <- function(c){
		out <- strsplit(c,"(\\s)+")
		as.vector(sapply(out, FUN = function(s)str_trim(s)))
	}
	
	#concatents a vector of params into csv form
	concParam <- function(c){
		paste(c, collapse=" ")
	}
	
	#removes leading/tailing whitespaces
	trim <- function(x) {
		gsub("^\\s+|\\s+$", "", x)
	}
	
	#helper methode to make printing to statusbar more readable
	status_print <- function(s,p="",...){
		statusbar$Push(statusbar$GetContextId(p), s)
	}
	
	#helper methode to simplyfie/more readable set widgets inactive depending on checkbox
	toggle <- function(a, b, invert=FALSE){
		if(invert == FALSE)lapply(a, function(s){s$SetSensitive(b$GetActive())})
		else lapply(a, function(s){s$SetSensitive(!b$GetActive())})
	}
	
	####################################################
	#END FUNCTIONS
	####################################################
	
	window.main$Resize(1100,700)
	
	#Table for timeseries
	renderer.ts$SetAlignment(0.5,0.5)
	column.ts$SetTitle("timeseries")
	column.ts$PackStart(renderer.ts)
	column.ts$SetAlignment(0.5)
	column.ts$AddAttribute(renderer.ts, "text", 0)
	table.ts$AppendColumn(column.ts)
	elements <- sapply(object@x12List, function(x) x@tsName)
	sapply(elements,
			function(string) {
				## Add a new row to the model
				iter <- table.model$Append()$iter
				table.model$Set(iter, 0, string)
			})
	table.ts$SetModel(table.model)
	table.ts$GetSelection()$SetMode("GTK_SELECTION_MULTIPLE")
	handler.tstable <- gSignalConnect(table.ts$GetSelection(), "changed", f=tablehandler)
	
	#history frame
	panel.history$PackStart(label.history, padding=3)
	panel.history$PackStart(combobox.history, padding=3)
	panel.history$PackStart(button.revert, padding=3)
	panel.history$PackStart(button.clearhistory, padding=3)
	frame.history$Add(panel.history)
	gSignalConnect(button.clearhistory, "released", f=function(...){
				dialog <- gtkMessageDialog(window.main, "destroy-with-parent",
						"warning", "yes-no", "History will be lost, do you really want to do this?")
				res <- dialog$Run()
				if(res==-8){
					object@x12List[[indices[1]]] <<- cleanHistory(object@x12List[[indices[1]]])
					make_history()
					update_notebook()
				}
				dialog$destroy()
			})
	gSignalConnect(button.revert, "released", f=function(...){
				dialog <- gtkMessageDialog(window.main, "destroy-with-parent",
						"warning", "yes-no", "Do you really want to revert the parameter?")
				res <- dialog$Run()
				if(res==-8){
					if(isNumber(combobox.history$GetActiveText())==TRUE) k <- as.numeric(combobox.history$GetActiveText())
					else if(combobox.history$GetActiveText()=="previous") k <- length(object@x12List[[indices[1]]]@x12OldParameter)
					if(k>0){
						object@x12List[[indices[1]]] <<- prev(object@x12List[[indices[1]]], n=k)
						make_history()
						update_notebook()
						status_print(capture.output(read_x12(object, indices)))	
						update_outliertable()
					}
					else{
						status_print("Nothing to revert!")
					}
				}
				dialog$destroy()
			})
	
	#######################################################
	#Column timeseries
	#######################################################
	gSignalConnect(button.update, "released", f=update_handler)
#	panel.ts$PackStart(button.update,expand=FALSE)
	panel.ts$PackStart(table.ts,expand=TRUE)
	panel.ts$PackStart(frame.history, expand=FALSE)
	
	#######################################################
	#Column x12-parameters
	#######################################################
	panel.params$SetRowSpacings(5)
	panel.params$SetColSpacings(2)
	#####spanframe
	#start
	panel.span$PackStart(checkb.spanactive, expand=FALSE)
	handler.spanactive <- gSignalConnect(checkb.spanactive, "toggled", f=checkbuttonhandler)
	panel.span$PackStart(checkb.spanstart)
	gSignalConnect(checkb.spanstart, "toggled", f=checkbuttonhandler)
	panel.spanstart$PackStart(label.spanstartyear)
	entry.spanstartyear$SetSizeRequest(30,-1)
	panel.spanstart$PackStart(entry.spanstartyear, expand=TRUE, padding=2)
	panel.spanstart$PackStart(label.spanstartperiod)
	entry.spanstartperiod$SetSizeRequest(30,-1)
	panel.spanstart$PackStart(entry.spanstartperiod, expand=TRUE, padding=2)
	panel.span$PackStart(panel.spanstart)
	handler.spanstartyear <- gSignalConnect(entry.spanstartyear, "changed", f=x12input_handler)
	handler.spanstartperiod <- gSignalConnect(entry.spanstartperiod, "changed", f=x12input_handler)
	
	#end
	panel.span$PackStart(checkb.spanend)
	gSignalConnect(checkb.spanend, "toggled", f=checkbuttonhandler)
	panel.spanend$PackStart(label.spanendyear)
	entry.spanendyear$SetSizeRequest(30,-1)
	panel.spanend$PackStart(entry.spanendyear, expand=TRUE, padding=2)
	panel.spanend$PackStart(label.spanendperiod)
	entry.spanendperiod$SetSizeRequest(30,-1)
	panel.spanend$PackStart(entry.spanendperiod, expand=TRUE, padding=2)
	panel.span$PackStart(panel.spanend, padding=3)
	handler.spanendyear <- gSignalConnect(entry.spanendyear, "changed", f=x12input_handler)
	handler.spanendperiod <- gSignalConnect(entry.spanendperiod, "changed", f=x12input_handler)
	
	frame.span$Add(panel.span)
	panel.params$AttachDefaults(frame.span, 1, 6, 1, 5)
	
	#####modelspanframe
	#start
	panel.modelspan$PackStart(checkb.modelspanactive, expand=FALSE)
	gSignalConnect(checkb.modelspanactive, "toggled", f=checkbuttonhandler)
	panel.modelspan$PackStart(checkb.modelspanstart)
	gSignalConnect(checkb.modelspanstart, "toggled", f=checkbuttonhandler)
	panel.modelspanstart$PackStart(label.modelspanstartyear)
	entry.modelspanstartyear$SetSizeRequest(30,-1)
	panel.modelspanstart$PackStart(entry.modelspanstartyear, expand=TRUE, padding=2)
	panel.modelspanstart$PackStart(label.modelspanstartperiod)
	entry.modelspanstartperiod$SetSizeRequest(30,-1)
	panel.modelspanstart$PackStart(entry.modelspanstartperiod, expand=TRUE, padding=2)
	panel.modelspan$PackStart(panel.modelspanstart)
	handler.modelspanstartyear <- gSignalConnect(entry.modelspanstartyear, "changed", f=x12input_handler)
	handler.modelspanstartperiod <- gSignalConnect(entry.modelspanstartperiod, "changed", f=x12input_handler)
	
	#end
	panel.modelspan$PackStart(checkb.modelspanend)
	gSignalConnect(checkb.modelspanend, "toggled", f=checkbuttonhandler)
	panel.modelspanend$PackStart(label.modelspanendyear)
	entry.modelspanendyear$SetSizeRequest(30,-1)
	panel.modelspanend$PackStart(entry.modelspanendyear, expand=TRUE, padding=2)
	panel.modelspanend$PackStart(label.modelspanendperiod)
	entry.modelspanendperiod$SetSizeRequest(30,-1)
	panel.modelspanend$PackStart(entry.modelspanendperiod, expand=TRUE, padding=2)
	panel.modelspan$PackStart(panel.modelspanend, padding=3)
	handler.modelspanendyear <- gSignalConnect(entry.modelspanendyear, "changed", f=x12input_handler)
	handler.modelspanendperiod <- gSignalConnect(entry.modelspanendperiod, "changed", f=x12input_handler)
	
	frame.modelspan$Add(panel.modelspan)
	panel.params$AttachDefaults(frame.modelspan, 1, 6, 6, 10)
	
	#####decimals
# panel.decimals$PackStart(checkb.decimalsactive)
# gSignalConnect(checkb.decimalsactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.decimals, 1, 3, 10, 11)
	label.decimals$SetAlignment(0,0.5)
	entry.decimals$SetSizeRequest(50,-1)
	panel.params$AttachDefaults(entry.decimals, 4, 6, 10, 11)
#	panel.params$AttachDefaults(panel.decimals)
	handler.decimals <- gSignalConnect(entry.decimals, "changed", f=x12input_handler)
	
	#####transform
# panel.transform$PackStart(checkb.transformactive) 
# gSignalConnect(checkb.transformactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.transform, 1, 3, 11, 12)
	label.transform$SetAlignment(0,0.5)
	combobox.transform$SetSizeRequest(57,-1)
	panel.params$AttachDefaults(combobox.transform,  4, 6, 11, 12)
	combobox.transform$AppendText("auto")
	combobox.transform$AppendText("log")
	combobox.transform$AppendText("non")
#	panel.params$AttachDefaults(panel.transform)
	handler.transform <- gSignalConnect(combobox.transform, "changed", f=comboboxx12handler)
	
	#####arima
	panel.params$AttachDefaults(checkb.arimaactive, 1, 2, 12, 13)
	gSignalConnect(checkb.arimaactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.arima, 2, 3, 12, 13)
	label.arima$SetAlignment(0,0.5)
	entry.arima1$SetSizeRequest(8,-1)
	panel.params$AttachDefaults(entry.arima1, 3, 4, 12, 13)
	entry.arima2$SetSizeRequest(8,-1)
	panel.params$AttachDefaults(entry.arima2, 4, 5, 12, 13)
	entry.arima3$SetSizeRequest(8,-1)
	panel.params$AttachDefaults(entry.arima3, 5, 6, 12, 13)
#	panel.params$AttachDefaults(panel.arima, expand=FALSE, padding=5)
	handler.arima1 <- gSignalConnect(entry.arima1, "changed", f=x12input_handler)
	handler.arima2 <- gSignalConnect(entry.arima2, "changed", f=x12input_handler)
	handler.arima3 <- gSignalConnect(entry.arima3, "changed", f=x12input_handler)
	
	#####sarima
	panel.params$AttachDefaults(checkb.sarimaactive, 1, 2, 13, 14)
	gSignalConnect(checkb.sarimaactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.sarima, 2, 3, 13, 14)
	label.sarima$SetAlignment(0,0.5)
	entry.sarima1$SetSizeRequest(8,-1)
	panel.params$AttachDefaults(entry.sarima1, 3, 4, 13, 14)
	entry.sarima2$SetSizeRequest(8,-1)
	panel.params$AttachDefaults(entry.sarima2, 4, 5, 13, 14)
	entry.sarima3$SetSizeRequest(8,-1)
	panel.params$AttachDefaults(entry.sarima3, 5, 6, 13, 14)
#	panel.params$AttachDefaults(panel.sarima, expand=FALSE, padding=5)
	handler.sarima1 <- gSignalConnect(entry.sarima1, "changed", f=x12input_handler)
	handler.sarima2 <- gSignalConnect(entry.sarima2, "changed", f=x12input_handler)
	handler.sarima3 <- gSignalConnect(entry.sarima3, "changed", f=x12input_handler)
	
	#####automdl, acceptdefault, balanced
	panel.divboxes$PackStart(checkb.automdl, expand=FALSE)
	panel.divboxes$PackStart(checkb.acceptdefault, expand=FALSE)
	panel.divboxes$PackStart(checkb.balanced, expand=FALSE)
	panel.divboxes$PackStart(checkb.seats, expand=FALSE)
	panel.divboxes$PackStart(checkb.estimate, expand=FALSE)
	panel.divboxes$PackStart(checkb.estOutofsample, expand=FALSE)
	panel.divboxes$PackStart(checkb.slidingspans, expand=FALSE)
	panel.divboxes$PackStart(checkb.onlytd, expand=FALSE)
	panel.divboxes$PackStart(checkb.sfshort, expand=FALSE)
	panel.divboxes$PackStart(checkb.x11appendfcst, expand=FALSE)
	panel.divboxes$PackStart(checkb.x11appendbcst, expand=FALSE)
	panel.divboxes$PackStart(checkb.x11excludefcst, expand=FALSE)
	panel.divboxes$PackStart(checkb.x11regress, expand=FALSE)
# panel.divboxes$PackStart(checkb.keep_x12out, expand=FALSE)
# panel.divboxes$PackStart(checkb.showWarnings, expand=FALSE)
	frame.divboxes$Add(panel.divboxes)
	panel.params$AttachDefaults(frame.divboxes, 1, 6, 14, 27)
	handlercheckb.automdl <- gSignalConnect(checkb.automdl, "toggled", checkbuttonx12handler)
	handlercheckb.acceptdefault <- gSignalConnect(checkb.acceptdefault, "toggled", checkbuttonx12handler)
	handlercheckb.balanced <- gSignalConnect(checkb.balanced, "toggled", checkbuttonx12handler)
	handlercheckb.seats <- gSignalConnect(checkb.seats, "toggled", checkbuttonx12handler)
	handlercheckb.estimate <- gSignalConnect(checkb.estimate, "toggled", checkbuttonx12handler)
	handlercheckb.estOutofsample <- gSignalConnect(checkb.estOutofsample, "toggled", checkbuttonx12handler)
	handlercheckb.slidingspans <- gSignalConnect(checkb.slidingspans, "toggled", checkbuttonx12handler)
	handlercheckb.onlytd <- gSignalConnect(checkb.onlytd, "toggled", checkbuttonx12handler)
	handlercheckb.sfshort <- gSignalConnect(checkb.sfshort, "toggled", checkbuttonx12handler)
	handlercheckb.x11appendfcst <- gSignalConnect(checkb.x11appendfcst, "toggled", checkbuttonx12handler)
	handlercheckb.x11appendbcst <- gSignalConnect(checkb.x11appendbcst, "toggled", checkbuttonx12handler)
	handlercheckb.x11excludefcst <- gSignalConnect(checkb.x11excludefcst, "toggled", checkbuttonx12handler)
	handlercheckb.x11regress <- gSignalConnect(checkb.x11regress, "toggled", checkbuttonx12handler)
	
	
	#####maxorder
# panel.maxorder$PackStart(checkb.maxorderactive)
# gSignalConnect(checkb.maxorderactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.maxorder, 1, 4, 27, 28)
	label.maxorder$SetAlignment(0,0.5)
	entry.maxorder1$SetSizeRequest(10,-1)
	panel.params$AttachDefaults(entry.maxorder1, 4, 5, 27, 28)
	entry.maxorder2$SetSizeRequest(10,-1)
	panel.params$AttachDefaults(entry.maxorder2, 5, 6, 27, 28)
#	panel.params$AttachDefaults(panel.maxorder, expand=FALSE, padding=5)
	handler.maxorder1 <- gSignalConnect(entry.maxorder1, "changed", f=x12input_handler)
	handler.maxorder2 <- gSignalConnect(entry.maxorder2, "changed", f=x12input_handler)
	
	#####maxdiff
# panel.maxdiff$PackStart(checkb.maxdiffactive)
# gSignalConnect(checkb.maxdiffactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.maxdiff, 1, 4, 28, 29)
	label.maxdiff$SetAlignment(0,0.5)
	entry.maxdiff1$SetSizeRequest(10,-1)
	panel.params$AttachDefaults(entry.maxdiff1, 4, 5, 28, 29)
	entry.maxdiff2$SetSizeRequest(10,-1)
	panel.params$AttachDefaults(entry.maxdiff2, 5, 6, 28, 29)
#	panel.params$AttachDefaults(panel.maxdiff, expand=FALSE, padding=5)
	handler.maxdiff1 <- gSignalConnect(entry.maxdiff1, "changed", f=x12input_handler)
	handler.maxdiff2 <- gSignalConnect(entry.maxdiff2, "changed", f=x12input_handler)
	
	####regvariables
	panel.params$AttachDefaults(checkb.regvariablesactive, 1, 2, 29, 30)
	gSignalConnect(checkb.regvariablesactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.regvariables, 2, 4, 29, 30)
	label.regvariables$SetAlignment(0, 0.5)
	entry.regvariables$SetSizeRequest(75,-1)
	panel.params$AttachDefaults(entry.regvariables, 4, 6, 29, 30)
#	panel.params$AttachDefaults(panel.regvariables, expand=FALSE, padding=5)
	handler.regvariables <- gSignalConnect(entry.regvariables, "changed", f=x12input_handler)
	
	####reguser
	panel.params$AttachDefaults(checkb.reguseractive, 1, 2, 30, 31)
	gSignalConnect(checkb.reguseractive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.reguser, 2, 4, 30, 31)
	label.reguser$SetAlignment(0, 0.5)
	entry.reguser$SetSizeRequest(53,-1)
	panel.params$AttachDefaults(entry.reguser, 4, 6, 30, 31)
#	panel.params$AttachDefaults(panel.reguser, expand=FALSE, padding=5)
	handler.reguser <- gSignalConnect(entry.reguser, "changed", f=x12input_handler)
	
	#####regfile
	panel.params$AttachDefaults(checkb.regfileactive, 1, 2, 31, 32)
	gSignalConnect(checkb.regfileactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.regfile, 2, 4, 31, 32)
	label.regfile$SetAlignment(0, 0.5)
	panel.params$AttachDefaults(filebutton.regfile, 4, 6, 31, 32)
	gSignalConnect(filebutton.regfile, "file-set", f=filebuttonhandler)
#	panel.params$AttachDefaults(panel.regfile, expand=FALSE, padding=5)
	
	####usertype
	panel.params$AttachDefaults(checkb.usertypeactive, 1, 2, 32, 33)
	gSignalConnect(checkb.usertypeactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.usertype, 2, 4, 32, 33)
	label.usertype$SetAlignment(0, 0.5)
	entry.usertype$SetSizeRequest(53,-1)
	panel.params$AttachDefaults(entry.usertype, 4, 6, 32, 33)
#	panel.params$AttachDefaults(panel.usertype, expand=FALSE, padding=5)
	handler.usertype <- gSignalConnect(entry.usertype, "changed", f=x12input_handler)
	
	#####centeruser
	panel.params$AttachDefaults(checkb.centeruseractive, 1, 2, 33, 34)
	gSignalConnect(checkb.centeruseractive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.centeruser, 2, 4, 33, 34)
	label.centeruser$SetAlignment(0,0.5)
	combobox.centeruser$SetSizeRequest(57,-1)
	panel.params$AttachDefaults(combobox.centeruser, 4, 6, 33, 34)
	combobox.centeruser$AppendText("mean")
	combobox.centeruser$AppendText("seasonal")
#	panel.params$AttachDefaults(panel.centeruser, expand=FALSE, padding=5)
	handler.centeruser <- gSignalConnect(combobox.centeruser, "changed", f=comboboxx12handler)
	
	#####regfilestart
	panel.regfilestart$PackStart(checkb.regfilestartactive, padding=5)
	gSignalConnect(checkb.regfilestartactive, "toggled", f=checkbuttonhandler)
	panel.regfilestartstart$PackStart(label.regfilestartstartyear)
	entry.regfilestartstartyear$SetSizeRequest(30,-1)
	panel.regfilestartstart$PackStart(entry.regfilestartstartyear)
	panel.regfilestartstart$PackStart(label.regfilestartstartperiod)
	entry.regfilestartstartperiod$SetSizeRequest(30,-1)
	panel.regfilestartstart$PackStart(entry.regfilestartstartperiod)
	panel.regfilestart$PackStart(panel.regfilestartstart, padding=5)
	frame.regfilestart$Add(panel.regfilestart)
	panel.params$AttachDefaults(frame.regfilestart, 1, 6, 34, 37)
	handler.regfilestartstartyear <- gSignalConnect(entry.regfilestartstartyear, "changed", f=x12input_handler)
	handler.regfilestartstartperiod <- gSignalConnect(entry.regfilestartstartperiod, "changed", f=x12input_handler)
	
# ####tblnames
# panel.tblnames$PackStart(checkb.tblnamesactive)
# gSignalConnect(checkb.tblnamesactive, "toggled", f=checkbuttonhandler)
# panel.tblnames$PackStart(label.tblnames)
# label.tblnames$SetAlignment(0, 0.5)
# entry.tblnames$SetSizeRequest(53,-1)
# panel.tblnames$PackStart(entry.tblnames)
# panel.params$AttachDefaults(panel.tblnames, expand=FALSE, padding=5)
# 
# ####Rtblnames
# panel.Rtblnames$PackStart(checkb.Rtblnamesactive)
# gSignalConnect(checkb.Rtblnamesactive, "toggled", f=checkbuttonhandler)
# panel.Rtblnames$PackStart(label.Rtblnames)
# label.Rtblnames$SetAlignment(0, 0.5)
# entry.Rtblnames$SetSizeRequest(53,-1)
# panel.Rtblnames$PackStart(entry.Rtblnames)
# panel.params$AttachDefaults(panel.Rtblnames, expand=FALSE, padding=5)
# 
# #####x12path
# panel.x12path$PackStart(checkb.x12pathactive)
# gSignalConnect(checkb.x12pathactive, "toggled", f=checkbuttonhandler)
# panel.x12path$PackStart(label.x12path)
# label.x12path$SetAlignment(0, 0.5)
# panel.x12path$PackStart(filebutton.x12path)
# panel.params$AttachDefaults(panel.x12path, expand=FALSE, padding=5)
# 
# #####x13path
# panel.x13path$PackStart(checkb.x13pathactive)
# gSignalConnect(checkb.x13pathactive, "toggled", f=checkbuttonhandler)
# panel.x13path$PackStart(label.x13path)
# label.x13path$SetAlignment(0, 0.5)
# panel.x13path$PackStart(filebutton.x13path)
# panel.params$AttachDefaults(panel.x13path, expand=FALSE, padding=5)
# 
# #####use
# panel.use$PackStart(checkb.useactive)
# gSignalConnect(checkb.useactive, "toggled", f=checkbuttonhandler)
# panel.use$PackStart(label.use)
# label.use$SetAlignment(0, 0.5)
# combobox.use$SetSizeRequest(57,-1)
# panel.use$PackStart(combobox.use, expand=TRUE)
# combobox.use$AppendText("x12")
# combobox.use$AppendText("x13")
# panel.params$AttachDefaults(panel.use, expand=FALSE, padding=5)
	
	####seatsparameter
	panel.params$AttachDefaults(checkb.seatsparameteractive, 1, 2, 37, 38)
	gSignalConnect(checkb.seatsparameteractive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.seatsparameter, 2, 4, 37, 38)
	label.seatsparameter$SetAlignment(0, 0.5)
	entry.seatsparameter$SetSizeRequest(53,-1)
	panel.params$AttachDefaults(entry.seatsparameter, 4, 6, 37, 38)
#	panel.params$AttachDefaults(panel.seatsparameter, expand=FALSE, padding=5)
	handler.seatsparameter <- gSignalConnect(entry.seatsparameter, "changed", f=x12input_handler)
	
	#####sigmalim
	panel.params$AttachDefaults(checkb.sigmalimactive, 1, 2, 38, 39)
	gSignalConnect(checkb.sigmalimactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.sigmalim, 2, 4, 38, 39)
	label.sigmalim$SetAlignment(0,0.5)
	entry.sigmalim1$SetSizeRequest(10,-1)
	panel.params$AttachDefaults(entry.sigmalim1, 4, 5, 38, 39)
	entry.sigmalim2$SetSizeRequest(10,-1)
	panel.params$AttachDefaults(entry.sigmalim2, 5, 6, 38, 39)
#	panel.params$AttachDefaults(panel.sigmalim, expand=FALSE, padding=5)
	handler.sigmalim1 <- gSignalConnect(entry.sigmalim1, "changed", f=x12input_handler)
	handler.sigmalim2<- gSignalConnect(entry.sigmalim2, "changed", f=x12input_handler)
	
	#####critical
	entry.criticalall$SetSizeRequest(30, -1)
	entry.criticalTC$SetSizeRequest(10, -1)
	entry.criticalAO$SetSizeRequest(10, -1)
	entry.criticalLS$SetSizeRequest(10, -1)
	gSignalConnect(radiob.criticalspecific, "toggled", f=checkbuttonhandler)
	gSignalConnect(radiob.criticalall, "toggled", f=checkbuttonhandler)
	gSignalConnect(checkb.criticalactive, "toggled", f=checkbuttonhandler)
	panel.critical$AttachDefaults(checkb.criticalactive, 1, 2, 1, 2)
	panel.critical$AttachDefaults(radiob.criticalall,1 , 2, 2, 3)
	panel.critical$AttachDefaults(radiob.criticalspecific, 1, 2, 4, 5)
	panel.critical$AttachDefaults(entry.criticalall, 2, 5, 2, 3)
	panel.critical$AttachDefaults(label.criticalAO, 2, 3, 3, 4)
	panel.critical$AttachDefaults(label.criticalLS, 3, 4, 3, 4)
	panel.critical$AttachDefaults(label.criticalTC, 4, 5, 3, 4)
	panel.critical$AttachDefaults(entry.criticalAO, 2, 3, 4, 5)
	panel.critical$AttachDefaults(entry.criticalLS, 3, 4, 4, 5)
	panel.critical$AttachDefaults(entry.criticalTC, 4, 5, 4, 5)
	frame.critical$Add(panel.critical)
	panel.params$AttachDefaults(frame.critical, 1, 6, 39, 44)
	handler.criticalall <- gSignalConnect(entry.criticalall, "changed", f=x12input_handler)
	handler.criticalAO <- gSignalConnect(entry.criticalAO, "changed", f=x12input_handler)
	handler.criticalLS <- gSignalConnect(entry.criticalLS, "changed", f=x12input_handler)
	handler.criticalTC <- gSignalConnect(entry.criticalTC, "changed", f=x12input_handler)
	
	#####outlier
#	panel.params$AttachDefaults(checkb.outlieractive, 1, 2, 44, 45) 
#	gSignalConnect(checkb.outlieractive, "toggled", f=checkbuttonhandler)
#	panel.params$AttachDefaults(label.outlier, 2, 4, 44, 45)
#	label.outlier$SetAlignment(0,0.5)
#	entry.outlier$SetSizeRequest(57,-1)
#	panel.params$AttachDefaults(entry.outlier, 4, 6, 44, 45)
	##	panel.params$AttachDefaults(panel.outlier, expand=FALSE, padding=5)
#	handler.outlier <- gSignalConnect(entry.outlier, "changed", f=x12input_handler)
	panel.outlier$AttachDefaults(checkb.outlieractive, 1, 2, 1, 2)
	panel.outlier$AttachDefaults(gtkLabel("all"), 1, 2, 2, 3)
	panel.outlier$AttachDefaults(gtkLabel("AO"), 2, 3, 2, 3)
	panel.outlier$AttachDefaults(gtkLabel("TC"), 3, 4, 2, 3)
	panel.outlier$AttachDefaults(gtkLabel("LS"), 4, 5, 2, 3)
	panel.outlier$AttachDefaults(checkb.outlierall, 1, 2, 3, 4)
	panel.outlier$AttachDefaults(checkb.outlierAO, 2, 3, 3, 4)
	panel.outlier$AttachDefaults(checkb.outlierTC, 3, 4, 3, 4)
	panel.outlier$AttachDefaults(checkb.outlierLS, 4, 5, 3, 4)
	frame.outlier$Add(panel.outlier)
	panel.params$AttachDefaults(frame.outlier, 1, 6, 44, 45)
	gSignalConnect(checkb.outlieractive, "toggled", f=checkbuttonhandler)
	handler.outlierall <- gSignalConnect(checkb.outlierall, "toggled", checkbuttonx12handler)
	handler.outlierTC <- gSignalConnect(checkb.outlierTC, "toggled", checkbuttonx12handler)
	handler.outlierAO <- gSignalConnect(checkb.outlierAO, "toggled", checkbuttonx12handler)
	handler.outlierLS <- gSignalConnect(checkb.outlierLS, "toggled", checkbuttonx12handler)
	
	
	#####outlierspan
	panel.params$AttachDefaults(checkb.outlierspanactive, 1, 2, 45, 46)
	gSignalConnect(checkb.outlierspanactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.outlierspan, 2, 4, 45, 46)
	label.outlierspan$SetAlignment(0,0.5)
	entry.outlierspan1$SetSizeRequest(10,-1)
	panel.params$AttachDefaults(entry.outlierspan1, 4, 5, 45, 46)
	entry.outlierspan2$SetSizeRequest(10,-1)
	panel.params$AttachDefaults(entry.outlierspan2, 5, 6, 45, 46)
#	panel.params$AttachDefaults(panel.outlierspan, expand=FALSE, padding=5)
	handler.outlierspan1 <- gSignalConnect(entry.outlierspan1, "changed", f=x12input_handler)
	handler.outlierspan2<- gSignalConnect(entry.outlierspan2, "changed", f=x12input_handler)
	
	#####outliermethod
	panel.params$AttachDefaults(checkb.outliermethodactive, 1, 2, 46, 47) 
	gSignalConnect(checkb.outliermethodactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.outliermethod, 2, 4, 46, 47)
	label.outliermethod$SetAlignment(0,0.5)
	combobox.outliermethod$SetSizeRequest(57,-1)
	panel.params$AttachDefaults(combobox.outliermethod, 4, 6, 46, 47)
	combobox.outliermethod$AppendText("addone")
	combobox.outliermethod$AppendText("addall")
#	panel.params$AttachDefaults(panel.outliermethod, expand=FALSE, padding=5)
	handler.outliermethod <- gSignalConnect(combobox.outliermethod, "changed", f=comboboxx12handler)
	
# ####file
# panel.file$PackStart(checkb.fileactive)
# gSignalConnect(checkb.fileactive, "toggled", f=checkbuttonhandler)
# panel.file$PackStart(label.file)
# label.file$SetAlignment(0, 0.5)
# entry.file$SetSizeRequest(53,-1)
# panel.file$PackStart(entry.file)
# panel.params$AttachDefaults(panel.file, expand=FALSE, padding=5)
	
	####forecast_years
	panel.params$AttachDefaults(checkb.forecast_yearsactive, 1, 2, 47, 48)
	gSignalConnect(checkb.forecast_yearsactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.forecast_years, 2, 4, 47, 48)
	label.forecast_years$SetAlignment(0, 0.5)
	entry.forecast_years$SetSizeRequest(53,-1)
	panel.params$AttachDefaults(entry.forecast_years, 4, 6, 47, 48)
#	panel.params$AttachDefaults(panel.forecast_years, expand=FALSE, padding=5)
	handler.forecast_years <- gSignalConnect(entry.forecast_years, "changed", f=x12input_handler)
	
	####backcast_years
	panel.params$AttachDefaults(checkb.backcast_yearsactive, 1, 2, 48, 49)
	gSignalConnect(checkb.backcast_yearsactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.backcast_years, 2, 4, 48, 49)
	label.backcast_years$SetAlignment(0, 0.5)
	entry.backcast_years$SetSizeRequest(53,-1)
	panel.params$AttachDefaults(entry.backcast_years, 4, 6, 48, 49)
#	panel.params$AttachDefaults(panel.backcast_years, expand=FALSE, padding=5)
	handler.backcast_years <- gSignalConnect(entry.backcast_years, "changed", f=x12input_handler)
	
	####forecast_conf
# panel.forecast_conf$PackStart(checkb.forecast_confactive)
# gSignalConnect(checkb.forecast_confactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.forecast_conf, 2, 4, 49, 50)
	label.forecast_conf$SetAlignment(0, 0.5)
	entry.forecast_conf$SetSizeRequest(53,-1)
	panel.params$AttachDefaults(entry.forecast_conf, 4, 6, 49, 50)
#	panel.params$AttachDefaults(panel.forecast_conf, expand=FALSE, padding=5)
	handler.forecast_conf <- gSignalConnect(entry.forecast_conf, "changed", f=x12input_handler)
	
	####aictest
	panel.params$AttachDefaults(checkb.aictestactive, 1, 2, 50, 51)
	gSignalConnect(checkb.aictestactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.aictest, 2, 4, 50, 51)
	label.aictest$SetAlignment(0, 0.5)
	entry.aictest$SetSizeRequest(53,-1)
	panel.params$AttachDefaults(entry.aictest, 4, 6, 50, 51)
#	panel.params$AttachDefaults(panel.aictest, expand=FALSE, padding=5)
	handler.aictest <- gSignalConnect(entry.aictest, "changed", f=x12input_handler)
	
	#####samode
	panel.params$AttachDefaults(checkb.samodeactive, 1, 2, 51, 52) 
	gSignalConnect(checkb.samodeactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.samode, 2, 4, 51, 52)
	label.samode$SetAlignment(0,0.5)
	combobox.samode$SetSizeRequest(57,-1)
	panel.params$AttachDefaults(combobox.samode, 4, 6, 51, 52)
	combobox.samode$AppendText("mult")
	combobox.samode$AppendText("add")
	combobox.samode$AppendText("pseudoadd")
	combobox.samode$AppendText("logadd")
#	panel.params$AttachDefaults(panel.samode, expand=FALSE, padding=5)
	handler.samode <- gSignalConnect(combobox.samode, "changed", f=comboboxx12handler)
	
	####seasonalma
	panel.params$AttachDefaults(checkb.seasonalmaactive, 1, 2, 52, 53)
	gSignalConnect(checkb.seasonalmaactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.seasonalma, 2, 4, 52, 53)
	label.seasonalma$SetAlignment(0, 0.5)
	entry.seasonalma$SetSizeRequest(53,-1)
	panel.params$AttachDefaults(entry.seasonalma, 4, 6, 52, 53)
#	panel.params$AttachDefaults(panel.seasonalma, expand=FALSE, padding=5)
	handler.seasonalma <- gSignalConnect(entry.seasonalma, "changed", f=x12input_handler)
	
	####trendma
	panel.params$AttachDefaults(checkb.trendmaactive, 1, 2, 53, 54)
	gSignalConnect(checkb.trendmaactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.trendma, 2, 4, 53, 54)
	label.trendma$SetAlignment(0, 0.5)
	entry.trendma$SetSizeRequest(53,-1)
	panel.params$AttachDefaults(entry.trendma, 4, 6, 53, 54)
#	panel.params$AttachDefaults(panel.trendma, expand=FALSE, padding=5)
	handler.trendma <- gSignalConnect(entry.trendma, "changed", f=x12input_handler)
	
	#####x11calendarsigma
	panel.params$AttachDefaults(checkb.x11calendarsigmaactive, 1, 2, 54, 55) 
	gSignalConnect(checkb.x11calendarsigmaactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.x11calendarsigma, 2, 4, 54, 55)
	label.x11calendarsigma$SetAlignment(0,0.5)
	combobox.x11calendarsigma$SetSizeRequest(57,-1)
	panel.params$AttachDefaults(combobox.x11calendarsigma, 4, 6, 54, 55)
	combobox.x11calendarsigma$AppendText("all")
	combobox.x11calendarsigma$AppendText("signif")
	combobox.x11calendarsigma$AppendText("select")
#	panel.params$AttachDefaults(panel.x11calendarsigma, expand=FALSE, padding=5)
	handler.x11calendarsigma <- gSignalConnect(combobox.x11calendarsigma, "changed", f=comboboxx12handler)
	
	#####x11final
# panel.x11final$PackStart(checkb.x11finalactive) 
# gSignalConnect(checkb.x11finalactive, "toggled", f=checkbuttonhandler)
	panel.params$AttachDefaults(label.x11final, 2, 4, 55, 56)
	label.x11final$SetAlignment(0,0.5)
	entry.x11final$SetSizeRequest(57,-1)
	panel.params$AttachDefaults(entry.x11final, 4, 6, 55, 56)
#	panel.params$AttachDefaults(panel.x11final, expand=FALSE, padding=5)
	handler.x11final <- gSignalConnect(entry.x11final, "changed", f=x12input_handler)
	
	#####TOOLTIPS
	frame.span$SetTooltipMarkup(string.span)
	frame.modelspan$SetTooltipMarkup(string.modelspan)
	label.decimals$SetTooltipMarkup(string.decimals)
	label.transform$SetTooltipMarkup(string.transform)
	label.arima$SetTooltipMarkup(string.arima)
	label.sarima$SetTooltipMarkup(string.sarima)
	checkb.automdl$SetTooltipMarkup(string.automdl)
	checkb.balanced$SetTooltipMarkup(string.balanced)
	checkb.seats$SetTooltipMarkup(string.seats)
	checkb.estimate$SetTooltipMarkup(string.estimate)
	checkb.estOutofsample$SetTooltipMarkup(string.estimateoutofsamples)
	checkb.slidingspans$SetTooltipMarkup(string.slidingspans)
	checkb.onlytd$SetTooltipMarkup(string.onlytd)
	checkb.sfshort$SetTooltipMarkup(string.sfshort)
	checkb.x11appendfcst$SetTooltipMarkup(string.x11appendfcst)
	checkb.x11appendbcst$SetTooltipMarkup(string.x11appendfbst)
	checkb.x11excludefcst$SetTooltipMarkup(string.x11excludefcst)
	checkb.x11regress$SetTooltipMarkup(string.x11regress)
	label.maxorder$SetTooltipMarkup(string.maxorder)
	label.maxdiff$SetTooltipMarkup(string.maxdiff)
	label.regvariables$SetTooltipMarkup(string.regvariables)
	label.reguser$SetTooltipMarkup(string.reguser)
	label.regfile$SetTooltipMarkup(string.regfile)
	label.usertype$SetTooltipMarkup(string.usertype)
	label.centeruser$SetTooltipMarkup(string.centeruser)
	frame.regfilestart$SetTooltipMarkup(string.regfilestart)
	label.seatsparameter$SetTooltipMarkup(string.seatsparameter)
	label.sigmalim$SetTooltipMarkup(string.sigmalim)
	frame.critical$SetTooltipMarkup(string.critical)
	label.outlier$SetTooltipMarkup(string.outlier)
	label.outlierspan$SetTooltipMarkup(string.outlierspan)
	label.outliermethod$SetTooltipMarkup(string.outliermethod)
	label.forecast_years$SetTooltipMarkup(string.forecast_years)
	label.backcast_years$SetTooltipMarkup(string.backcast_years)
	label.forecast_conf$SetTooltipMarkup(string.forecast_conf)
	label.aictest$SetTooltipMarkup(string.aictest)
	label.samode$SetTooltipMarkup(string.samode)
	label.seasonalma$SetTooltipMarkup(string.seasonalma)
	label.trendma$SetTooltipMarkup(string.trendma)
	label.x11calendarsigma$SetTooltipMarkup(string.x11calendarsigma)
	label.x11final$SetTooltipMarkup(string.x11final)
	
	
	
	#######################################################
	#Column plot-parameters
	#######################################################
	#manualoutlier table
	renderer.manualoutliertype$SetAlignment(0.5, 0.5)
	column.manualoutliertype$SetTitle("Type")
	column.manualoutliertype$PackStart(renderer.manualoutliertype)
	column.manualoutliertype$SetAlignment(0.5)
	column.manualoutliertype$AddAttribute(renderer.manualoutliertype, "text", 0)
	renderer.manualoutlieryear$SetAlignment(0.5, 0.5)
	column.manualoutlieryear$SetTitle("Year")
	column.manualoutlieryear$PackStart(renderer.manualoutlieryear)
	column.manualoutlieryear$SetAlignment(0.5)
	column.manualoutlieryear$AddAttribute(renderer.manualoutlieryear, "text", 1)
	renderer.manualoutlierperiod$SetAlignment(0.5, 0.5)
	column.manualoutlierperiod$SetTitle("period")
	column.manualoutlierperiod$PackStart(renderer.manualoutlierperiod)
	column.manualoutlierperiod$SetAlignment(0.5)
	column.manualoutlierperiod$AddAttribute(renderer.manualoutlierperiod, "text", 2)
	table.manualoutlier$AppendColumn(column.manualoutliertype)
	table.manualoutlier$AppendColumn(column.manualoutlieryear)
	table.manualoutlier$AppendColumn(column.manualoutlierperiod)
#	sapply(outlierlist,
#			function(string) {
#				## Add a new row to the model
#				iter <- tablemodel.manualoutlier$Append()$iter
#				tablemodel.manualoutlier$Set(iter, 0, string[1], 1, string[2], 2, string[3])
#			})
	update_outliertable()
	table.manualoutlier$SetModel(tablemodel.manualoutlier)
	#manualoutlier panel
	table.manualoutlier$SetSizeRequest(-1, 180)
	scroll.manualoutlier$Add(table.manualoutlier)
	panel.manualoutlier$AttachDefaults(scroll.manualoutlier, 1, 3, 1, 2)
	panel.manualoutlier$AttachDefaults(button.manualoutlierremove, 1, 3, 2, 3)
	panel.manualoutlier$AttachDefaults(label.manualoutliertype, 1, 2, 3, 4)
	combobox.manualoutliertype$AppendText("TC")
	combobox.manualoutliertype$AppendText("LS")
	combobox.manualoutliertype$AppendText("AO")
	panel.manualoutlier$AttachDefaults(combobox.manualoutliertype, 2, 3, 3, 4)
	panel.manualoutlier$AttachDefaults(label.manualoutlieryear, 1, 2, 4, 5)
	entry.manualoutlieryear$SetSizeRequest(20,-1)
	entry.manualoutlierperiod$SetSizeRequest(20,-1)
	panel.manualoutlier$AttachDefaults(entry.manualoutlieryear, 2, 3, 4, 5)
	panel.manualoutlier$AttachDefaults(label.manualoutlierperiod, 1, 2, 5, 6)
	panel.manualoutlier$AttachDefaults(entry.manualoutlierperiod, 2, 3, 5, 6)
	panel.manualoutlier$AttachDefaults(button.manualoutlieradd, 1, 3, 6, 7)
	panel.manualoutlier$AttachDefaults(button.manualoutlieraddclick, 1, 3, 7, 8)
	frame.manualoutlier$Add(panel.manualoutlier)
	panel.plotp$PackStart(frame.manualoutlier, expand=FALSE)
	gSignalConnect(button.manualoutlierremove, "released", f=manualoutlierhandler)
	gSignalConnect(button.manualoutlieraddclick, "released", f=manualoutlierhandler)
	gSignalConnect(button.manualoutlieradd, "released", f=manualoutlierhandler)
	
	
	#plot(...)
	panel.plotparams$PackStart(checkb.sa)
	panel.plotparams$PackStart(checkb.trend)
	panel.plotparams$PackStart(checkb.logtransform)
	panel.plotparams$PackStart(checkb.showCI)
	#panel.plotparams$PackStart(checkb.showLine)
	panel.plotparams$PackStart(checkb.pointsOriginal)
	panel.plotparams$PackStart(checkb.showAllout)
# panel.plotparams$PackStart(checkb.showAlloutLines)
# panel.plotparams$PackStart(checkb.annComp)
# panel.plotparams$PackStart(checkb.annCompTrend)
# gSignalConnect(checkb.original, "toggled", f=function(...) update_notebook())
	gSignalConnect(checkb.sa, "toggled", f=function(...) update_notebook(onlyplot=TRUE))
	gSignalConnect(checkb.trend, "toggled", f=function(...) update_notebook(onlyplot=TRUE))
	gSignalConnect(checkb.logtransform, "toggled", f=function(...) update_notebook(onlyplot=TRUE))
	gSignalConnect(checkb.showCI, "toggled", f=function(...) update_notebook(onlyplot=TRUE))
	#gSignalConnect(checkb.showLine, "toggled", f=function(...) update_notebook(onlyplot=TRUE))
	gSignalConnect(checkb.pointsOriginal, "toggled", f=function(...) update_notebook(onlyplot=TRUE))
	gSignalConnect(checkb.showAllout, "toggled", f=function(...) update_notebook())
# gSignalConnect(checkb.showAlloutLines, "toggled", f=function(...) update_notebook())
# gSignalConnect(checkb.annComp, "toggled", f=function(...) update_notebook())
# gSignalConnect(checkb.annCompTrend, "toggled", f=function(...) update_notebook())
	##showout panel
#	panel.showout$SetColSpacings(5)
#	panel.showout$SetRowSpacings(5)
	entry.showoutyear$SetSizeRequest(15,-1)
	entry.showoutperiod$SetSizeRequest(15,-1)
	#combobox.showouttype$SetSizeRequest(15,-1)
	#combobox.showouttype$AppendText("AO")
	#combobox.showouttype$AppendText("LS")
	#combobox.showouttype$AppendText("TC")
	panel.showout$AttachDefaults(checkb.showout, 1, 2, 1, 2)
	panel.showout$AttachDefaults(label.showoutyear, 1, 2, 3, 4)
	panel.showout$AttachDefaults(entry.showoutyear, 2, 3, 3, 4)
	panel.showout$AttachDefaults(label.showoutperiod, 1, 2, 4, 5)
	panel.showout$AttachDefaults(entry.showoutperiod, 2, 3, 4, 5)
	#panel.showout$AttachDefaults(label.showouttype, 1, 2, 2, 3)
	#panel.showout$AttachDefaults(combobox.showouttype, 2, 3, 2, 3)
	alignment.showout <- gtkAlignment()
	alignment.showout$SetPadding(3,3,3,3)
	alignment.showout$Add(panel.showout)
	frame.showout$Add(alignment.showout)
	panel.plotparams$PackStart(frame.showout, padding=3)
	gSignalConnect(checkb.showout, "toggled", f=function(...){
				update_notebook(onlyplot=TRUE)
				toggle(c(entry.showoutyear), checkb.showout)
				#toggle(c(combobox.showouttype), checkb.showout)
				toggle(c(entry.showoutperiod), checkb.showout)})
	gSignalConnect(entry.showoutyear, "changed", f=function(...) update_notebook(onlyplot=TRUE))
	gSignalConnect(entry.showoutperiod, "changed", f=function(...) update_notebook(onlyplot=TRUE))
	#gSignalConnect(combobox.showouttype, "changed", f=function(...) update_notebook(onlyplot=TRUE))
	entry.showoutyear$SetSensitive(FALSE)
	entry.showoutperiod$SetSensitive(FALSE)
	#combobox.showouttype$SetSensitive(FALSE)
	checkb.showout$SetActive(FALSE)
	##
	frame.plotparams$Add(panel.plotparams)
	panel.plotp$PackStart(frame.plotparams,expand=FALSE)
	
	#spectral frame
	radiob.spectralsa$SetActive(TRUE)
	panel.spectral$PackStart(radiob.spectralsa)
	panel.spectral$PackStart(radiob.spectraloriginal)
	panel.spectral$PackStart(radiob.spectralirregular)
	panel.spectral$PackStart(radiob.spectralresiduals)
	gSignalConnect(radiob.spectralsa, "toggled", f=function(...) if(radiob.spectralsa$GetActive()==TRUE)update_notebook(onlyplot=TRUE))
	gSignalConnect(radiob.spectraloriginal, "toggled", f=function(...) if(radiob.spectraloriginal$GetActive()==TRUE)update_notebook(onlyplot=TRUE))
	gSignalConnect(radiob.spectralirregular, "toggled", f=function(...) if(radiob.spectralirregular$GetActive()==TRUE)update_notebook(onlyplot=TRUE))
	gSignalConnect(radiob.spectralresiduals, "toggled", f=function(...) if(radiob.spectralresiduals$GetActive()==TRUE)update_notebook(onlyplot=TRUE))
	frame.spectral$Add(panel.spectral)
	panel.plotp$PackStart(frame.spectral, expand=FALSE)
	
	#seasonal factors plot
	radiob.rsdacfacf$SetActive(TRUE)
	panel.rsdacf$PackStart(radiob.rsdacfacf)
	panel.rsdacf$PackStart(radiob.rsdacfpacf)
	panel.rsdacf$PackStart(radiob.rsdacfacf2)
	gSignalConnect(radiob.rsdacfacf, "toggled", f=function(...) if(radiob.rsdacfacf$GetActive()==TRUE)update_notebook(onlyplot=TRUE))
	gSignalConnect(radiob.rsdacfpacf, "toggled", f=function(...) if(radiob.rsdacfpacf$GetActive()==TRUE)update_notebook(onlyplot=TRUE))
	gSignalConnect(radiob.rsdacfacf2, "toggled", f=function(...) if(radiob.rsdacfacf2$GetActive()==TRUE)update_notebook(onlyplot=TRUE))
	frame.rsdacf$Add(panel.rsdacf)
	panel.plotp$PackStart(frame.rsdacf, expand=FALSE)
	
	#summary parameter
	panel.summaryparameter$PackStart(checkb.fullSummary)
	panel.summaryparameter$PackStart(checkb.spectraldetail)
	panel.summaryparameter$PackStart(checkb.almostout)
	panel.summaryparameter$PackStart(checkb.rsdautocorr)
	panel.summaryparameter$PackStart(checkb.q2)
	panel.summaryparameter$PackStart(checkb.likelihoodstat)
	panel.summaryparameter$PackStart(checkb.aape)
	panel.summaryparameter$PackStart(checkb.idrsdseas)
	gSignalConnect(checkb.fullSummary, "toggled", f=function(...) update_notebook())
	gSignalConnect(checkb.spectraldetail, "toggled", f=function(...) update_notebook())
	gSignalConnect(checkb.almostout, "toggled", f=function(...) update_notebook())
	gSignalConnect(checkb.rsdautocorr, "toggled", f=function(...) update_notebook())
	gSignalConnect(checkb.q2, "toggled", f=function(...) update_notebook())
	gSignalConnect(checkb.likelihoodstat, "toggled", f=function(...) update_notebook())
	gSignalConnect(checkb.aape, "toggled", f=function(...) update_notebook())
	gSignalConnect(checkb.idrsdseas, "toggled", f=function(...) update_notebook())
	frame.summaryparameter$Add(panel.summaryparameter)
	panel.plotp$PackStart(frame.summaryparameter)
	
	#plotFbcast(...)
# panel.plotFbcastparams$PackStart(checkb.forecast)
# panel.plotFbcastparams$PackStart(checkb.backcast)
#  panel.plotparams$PackStart(checkb.showCI)
	##  panel.plotFbcastparams$PackStart(checkb.logtransform_fb)
#  panel.plotparams$PackStart(checkb.showLine)
#  panel.plotparams$PackStart(checkb.pointsOriginal)
	## gSignalConnect(checkb.forecast,"toggled", f=function(...) update_notebook(onlyplot=TRUE))
	## gSignalConnect(checkb.backcast, "toggled", f=function(...) update_notebook(onlyplot=TRUE))
#  gSignalConnect(checkb.showCI, "toggled", f=function(...) update_notebook(onlyplot=TRUE))
	##  gSignalConnect(checkb.logtransform_fb, "toggled", f=function(...) update_notebook(onlyplot=TRUE))
#  gSignalConnect(checkb.showLine, "toggled", f=function(...) update_notebook(onlyplot=TRUE))
#  gSignalConnect(checkb.pointsOriginal, "toggled", f=function(...) update_notebook(onlyplot=TRUE))
#  frame.plotFbcastparams$Add(panel.plotFbcastparams)
#  panel.plotp$PackStart(frame.plotFbcastparams, expand=FALSE)
	
	panel.gui$PackStart(panel.ts)
	panel.scrolledparams$SetPolicy("GTK_POLICY_NEVER","GTK_POLICY_ALWAYS")
	panel.scrolledparams$AddWithViewport(panel.params)
	panel.gui$PackStart(panel.scrolledparams)
	panel.scrolledplotparams$SetPolicy("GTK_POLICY_NEVER","GTK_POLICY_ALWAYS")
	panel.scrolledplotparams$AddWithViewport(panel.plotp)
	panel.gui$PackStart(panel.scrolledplotparams)
	
	#for each plot tab: frame + drawingarea
	frame.plot1$Add(area.plot1)
	frame.plot2$Add(area.plot2)
	frame.plot3$Add(area.plot3)
	frame.plot4$PackStart(area.plot4, expand=TRUE)
# frame.plot5$Add(area.plot5)
	
	#sliders for frame plot
	gSignalConnect(slider.plotmax, "value-changed", f=sliderhandler)
	gSignalConnect(slider.plotmin, "value-changed", f=sliderhandler)
	gSignalConnect(slider.plotmin, "format-value", f=sliderformat)
	gSignalConnect(slider.plotmax, "format-value", f=sliderformat)
	frame.plot4$PackStart(slider.plotmin, expand=FALSE)
	frame.plot4$PackStart(slider.plotmax, expand=FALSE)
	
	#contextmenu for plots
	menu.contextplotall$Attach(menuitem.saveaspdf, 0, 1, 0, 1)
	gSignalConnect(menuitem.saveaspdf, "activate", f=menuhandler)
	gSignalConnect(area.plot1, "button_release_event", f=mousehandlerdrawing)
	gSignalConnect(area.plot2, "button_release_event", f=mousehandlerdrawing)
	gSignalConnect(area.plot3, "button_release_event", f=mousehandlerdrawing)
	gSignalConnect(area.plot4, "button_release_event", f=mousehandlerdrawing)
	
	menu.contextplotwithoutlier$Attach(menuitem.saveaspdfwithoutlier, 0, 1, 0, 1)
	menu.contextplotwithoutlier$Attach(menuitem.addTC, 0, 1, 1, 2)
	menu.contextplotwithoutlier$Attach(menuitem.addLS, 0, 1, 2, 3)
	menu.contextplotwithoutlier$Attach(menuitem.addAO, 0, 1, 3, 4)
	gSignalConnect(menuitem.addTC, "activate", f=menuhandler)
	gSignalConnect(menuitem.addAO, "activate", f=menuhandler)
	gSignalConnect(menuitem.addLS, "activate", f=menuhandler)
	gSignalConnect(menuitem.saveaspdfwithoutlier, "activate", f=menuhandler)
	
	
	#summary tab setup
# buffer.summary$SetText(paste(capture.output(summary(x12)), collapse="\n"))
#	make_summary(object)
#	textview.summary$SetBuffer(buffer.summary)
#	textview.summary$setEditable(FALSE)
#	frame.summary$add(textview.summary)
#	table.summary <- setupSummarytable(table.summary, s)
#	i <- 0
#	sumnames <- names(getMethod("summary","x12Batch")(object, print=FALSE))
#	sumnames[1] <- "Value"
#	for(s in sumnames){
#		renderer <- gtkCellRendererText()
#		column <- gtkTreeViewColumn()
#		renderer$SetAlignment(0.5, 0.5)
#		column$SetTitle(s)
#		column$PackStart(renderer)
#		column$SetAlignment(0.5)
#		column$SetExpand(TRUE)
#		column$AddAttribute(renderer, "text", i)
#		if(i==0)column$AddAttribute(renderer, "background", length(sumnames))
#		else column$AddAttribute(renderer, "background", length(sumnames)+1)
#		i <- i + 1
#		table.summary$AppendColumn(column)
#		if(class(columns.summary)!="list")columns.summary <- list(column)
#		else columns.summary <- append(columns.summary, column)
#	}
	setup_summarytable(object)
	table.summary$SetModel(model.summary)
	table.summary$SetHeadersVisible(FALSE)
	frame.summary$add(table.summary)
	
	#summarytotal tab setup
	make_summary(object)
#	buffer.summarytotal$SetText(paste(capture.output(getMethod("summary","x12Batch")(object)), collapse="\n"))
	textview.summarytotal$SetBuffer(buffer.summarytotal)
	textview.summarytotal$setEditable(FALSE)
	frame.summarytotal$add(textview.summarytotal)
	
	
	notebook.plot$AppendPage(frame.plot1, tab.label=gtkLabel("autocorrelations of the residuals"))
	notebook.plot$AppendPage(frame.plot2, tab.label=gtkLabel("spectral"))
	notebook.plot$AppendPage(frame.plot3, tab.label=gtkLabel("seasonal factors"))
	notebook.plot$AppendPage(frame.plot4, tab.label=gtkLabel("plot"))
# notebook.plot$AppendPage(frame.plot5, tab.label=gtkLabel("Summary Total"))
	notebook.plot$AppendPage(frame.summarytotal, tab.label=gtkLabel("summary text"))
	notebook.plot$AppendPage(frame.summary, tab.label=gtkLabel("summary table"))
	
	gSignalConnect(menuitem.x12update, "activate", f=menuhandler)
	gSignalConnect(menuitem.x12loadp, "activate", f=menuhandler)
	gSignalConnect(menuitem.x12save, "activate", f=menuhandler)
	gSignalConnect(menuitem.x12savep, "activate", f=menuhandler)
	gSignalConnect(menuitem.x12load, "activate", f=menuhandler)
	gSignalConnect(menuitem.expplotaspdf, "activate", f=menuhandler)
	gSignalConnect(menuitem.expplotaspng, "activate", f=menuhandler)
	gSignalConnect(menuitem.expsummarycsv, "activate", f=menuhandler)
	gSignalConnect(menuitem.expsummaryclipboard, "activate", f=menuhandler)
	accgroup <- gtkAccelGroupNew()
	window.main$AddAccelGroup(accgroup)
	menuitem.x12update$AddAccelerator("activate", accgroup, GDK_U, "GDK_CONTROL_MASK", "GTK_ACCEL_VISIBLE")
	menu.export$Append(menuitem.expplotaspdf)
	menu.export$Append(menuitem.expplotaspng)
	menu.export$Append(menuitem.expsummarycsv)
	menu.export$Append(menuitem.expsummaryclipboard)
	menuitem.export$SetSubmenu(menu.export)
	menu.x12$Append(menuitem.x12update)
	menu.x12$Append(menuitem.x12loadp)
	menu.x12$Append(menuitem.x12savep)
	menu.x12$Append(menuitem.x12load)
	menu.x12$Append(menuitem.x12save)
	menuitem.x12$SetSubmenu(menu.x12)
	menubar.main$Append(menuitem.x12)
	menubar.main$Append(menuitem.export)
	
	panel.main$Add(panel.gui)
	panel.main$Add(notebook.plot)
	panel.window$PackStart(menubar.main, expand=FALSE)
	panel.window$PackStart(panel.main, expand=TRUE)
	panel.window$PackStart(statusbar, expand=FALSE)
	window.main$Add(panel.window)
	
	asCairoDevice(area.plot1)
	Sys.sleep(.1)
	
	#preloading the plots of the notebook tab
	notebook.plot$SetCurrentPage(1)
	notebook.plot$SetCurrentPage(2)
	notebook.plot$SetCurrentPage(3)
# notebook.plot$SetCurrentPage(4)
	notebook.plot$SetCurrentPage(0)
	notebook.plot$SetCurrentPage(5)
	
	gSignalConnect(notebook.plot, "switch-page", f=notebookhandler)
	window.main$Resize(1100,700)
	window.main$Show()
	read_x12(object, c(1))
	window.main$SetFocus(table.ts)
	table.ts$GetSelection()$SelectPath(gtkTreePathNewFirst())
	table.ts$SetCursor(gtkTreePathNewFirst())
	gSignalConnect(window.main, "destroy", f=function(...){gtkMainQuit()})
	status_print("Programm started!")
	gtkMain()
	return(object)
}

#library(x12)
#require(RGtk2)
#require(cairoDevice)
#data(AirPassengers)
#data(nottem)
#x12path <- "D:/Studium/Baccarbeit/x12a/x12a.exe"
###x12path <- "d:/x12/x12a.exe"
#setwd("d:/test")
##x12path <- "/usr/bin/x12a"
##x12path <- "d:/x12/x12a.exe"
##setwd("~/workspace/temp")
## s <- new("x12Single",ts=AirPassengers,tsName="air")
## s <- setP(s,list(estimate=TRUE,regvariables="AO1950.1",outlier="all",critical=list(LS=3.5,TC=2.5)))
## s <- X12(s)
#
#xb <- new("x12Batch",list(AirPassengers,AirPassengers,AirPassengers, nottem))
#xb <- setP(xb, list(transform="log", outlier="all"))
#xb <- setP(xb, list(transform="log", outlier="AO"), 3)
#xb <- setP(xb, list(transform="auto", centeruser="mean", sigmalim=c(1,2)),3)
##xb <- setP(xb, list(outlier=c("all", "AO", "TC")))
#
#xb <- setP(xb,list(automdl=FALSE))
#xb <- setP(xb,list(arima=c(0,0,0),sarima=c(1,1,0), balanced=FALSE),1)
#xb <- setP(xb,list(arima=c(1,2,3),sarima=c(1,1,0), balanced=FALSE),2)
#xb <- setP(xb,list(arima=c(1,1,0),sarima=c(1,1,0), span=c(1921,1,1928,3), forecast_years=2), 4)
#xb <- X12(xb)
#xb <- setP(xb,list(arima=c(1,1,1),sarima=c(1,1,1)),1)
#xb <- setP(xb,list(critical=5, regvariables=c("AO1950.1","AO1953.5","LS1955.1","lpyear"), outlier="all"),2)
#xb <- X12(xb)
#
#
#xbn <- x12GUI(xb)






