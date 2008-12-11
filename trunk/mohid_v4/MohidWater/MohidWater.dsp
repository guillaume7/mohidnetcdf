# Microsoft Developer Studio Project File - Name="MohidWater" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=MohidWater - Win32 Debug_MPI
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "MohidWater.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MohidWater.mak" CFG="MohidWater - Win32 Debug_MPI"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MohidWater - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "MohidWater - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "MohidWater - Win32 Debug_MPI" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "MohidWater - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /architecture:pn4 /check:bounds /check:power /check:overflow /check:underflow /compile_only /debug:none /fpp /include:"../Mohid_Base_1/Release" /include:"../Mohid_Base_2/Release" /nologo /stand:f95 /traceback /tune:pn4 /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused /fast /include:"/Libs"
# SUBTRACT F90 /check:arg_temp_created /check:output_conversion /fpscomp:filesfromcmd /fpscomp:ioformat /fpscomp:symbols /warn:nofileopt /warn:nouninitialized
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "../Libs" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x816 /d "NDEBUG"
# ADD RSC /l 0x816 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 Mohid_Base_1.lib Mohid_Base_2.lib kernel32.lib hdf5.lib hdf5_fortran.lib hdf5_hl.lib szlib.lib zlib.lib /nologo /stack:0x3d090000,0x3d090000 /subsystem:console /pdb:none /machine:I386 /libpath:"../Libs" /libpath:"../Mohid_Base_1/Release" /libpath:"../Mohid_Base_2/Release" /LARGEADDRESSAWARE

!ELSEIF  "$(CFG)" == "MohidWater - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /architecture:pn4 /automatic /check:bounds /check:format /check:power /check:output_conversion /check:overflow /check:underflow /compile_only /dbglibs /debug:full /d_lines /fpconstant /fpp /include:"../Mohid_Base_1/Debug" /include:"../Mohid_Base_2/Debug" /nologo /stand:f95 /traceback /tune:pn4 /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused /include:"/Libs"
# SUBTRACT F90 /check:arg_temp_created /fpscomp:filesfromcmd /fpscomp:ioformat /fpscomp:symbols /list /recursive /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "../Libs" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x816 /d "_DEBUG"
# ADD RSC /l 0x816 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 Mohid_Base_1.lib Mohid_Base_2.lib kernel32.lib hdf5.lib hdf5_fortran.lib hdf5_hl.lib szlib.lib zlib.lib /nologo /stack:0x3d090000 /subsystem:console /incremental:no /map /debug /machine:I386 /pdbtype:sept /libpath:"../Libs" /libpath:"../Mohid_Base_1/Debug" /libpath:"../Mohid_Base_2/Debug" /LARGEADDRESSAWARE
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "MohidWater - Win32 Debug_MPI"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "MohidWater___Win32_Debug_MPI"
# PROP BASE Intermediate_Dir "MohidWater___Win32_Debug_MPI"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug_MPI"
# PROP Intermediate_Dir "Debug_MPI"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /check:power /check:overflow /check:underflow /compile_only /dbglibs /debug:full /fpp /include:"../Mohid_Base_1/Debug" /include:"../Mohid_Base_2/Debug" /nologo /stand:f95 /traceback /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused /include:"/Libs"
# SUBTRACT BASE F90 /fpscomp:filesfromcmd /fpscomp:ioformat /fpscomp:symbols /warn:nofileopt
# ADD F90 /check:bounds /check:power /check:overflow /check:underflow /compile_only /dbglibs /debug:full /define:"_USE_MPI" /fpp /include:"../Mohid_Base_1/Debug_MPI" /include:"../Mohid_Base_2/Debug_MPI" /nologo /stand:f95 /traceback /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused /include:"/Libs"
# SUBTRACT F90 /fpscomp:filesfromcmd /fpscomp:ioformat /fpscomp:symbols /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /I "../Libs" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "../Libs" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x816 /d "_DEBUG"
# ADD RSC /l 0x816 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 Mohid_Base_1.lib Mohid_Base_2.lib kernel32.lib hdf5.lib hdf5_fortran.lib hdf5_hl.lib szlib.lib zlib.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept /libpath:"../Libs" /libpath:"../Mohid_Base_1/Debug" /libpath:"../Mohid_Base_2/Debug"
# ADD LINK32 Mohid_Base_1.lib Mohid_Base_2.lib kernel32.lib hdf5.lib hdf5_fortran.lib hdf5_hl.lib szlib.lib zlib.lib fmpich2s.lib /nologo /stack:0x3d09000,0x3d09000 /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept /libpath:"../Libs" /libpath:"../Mohid_Base_1/Debug_MPI" /libpath:"../Mohid_Base_2/Debug_MPI"

!ENDIF 

# Begin Target

# Name "MohidWater - Win32 Release"
# Name "MohidWater - Win32 Debug"
# Name "MohidWater - Win32 Debug_MPI"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\Main.F90
DEP_F90_MAIN_=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	".\mpif.f90"\
	".\Release\ModuleHydrodynamic.mod"\
	".\Release\ModuleModel.mod"\
	".\Release\ModuleWaterProperties.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleAssimilation.F90
DEP_F90_MODUL=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleFillMatrix.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleConsolidation.F90
DEP_F90_MODULE=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleBoxDif.mod"\
	"..\Mohid_Base_2\Release\ModuleFillMatrix.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleFreeVerticalMovement.F90
DEP_F90_MODULEF=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleGauge.F90
DEP_F90_MODULEG=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	".\Release\ModuleToga.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleGOTM.F90
DEP_F90_MODULEGO=\
	".\GOTMVariables_in.f90"\
	".\GOTMVariables_out.f90"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleHydrodynamic.f90
DEP_F90_MODULEH=\
	"..\Mohid_Base_1\Release\ModuleDischarges.mod"\
	"..\Mohid_Base_1\Release\ModuleDrawing.mod"\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleProfile.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleBoxDif.mod"\
	"..\Mohid_Base_2\Release\ModuleFillMatrix.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	"..\Mohid_Base_2\Release\ModuleStatistic.mod"\
	".\mpif.f90"\
	".\Release\ModuleAssimilation.mod"\
	".\Release\ModuleHydrodynamicFile.mod"\
	".\Release\ModuleOpenBoundary.mod"\
	".\Release\ModuleTurbulence.mod"\
	".\Release\ModuleWaves.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleHydrodynamicFile.F90
DEP_F90_MODULEHY=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHydroIntegration.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleInterfaceSedimentWater.F90
DEP_F90_MODULEI=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleInterface.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleBoxDif.mod"\
	"..\Mohid_Base_2\Release\ModuleFillMatrix.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	"..\Mohid_Base_2\Release\ModuleStatistic.mod"\
	".\Release\ModuleConsolidation.mod"\
	".\Release\ModuleFreeVerticalMovement.mod"\
	".\Release\ModuleHydrodynamic.mod"\
	".\Release\ModuleLagrangian.mod"\
	".\Release\ModuleLagrangianGlobal.mod"\
	".\Release\ModuleSand.mod"\
	".\Release\ModuleSedimentProperties.mod"\
	".\Release\ModuleTurbGOTM.mod"\
	".\Release\ModuleTurbulence.mod"\
	".\Release\ModuleWaterProperties.mod"\
	".\Release\ModuleWaves.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleInterfaceWaterAir.F90
DEP_F90_MODULEIN=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleAtmosphere.mod"\
	"..\Mohid_Base_2\Release\ModuleBoxDif.mod"\
	"..\Mohid_Base_2\Release\ModuleFillMatrix.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	".\Release\ModuleHydrodynamic.mod"\
	".\Release\ModuleLagrangian.mod"\
	".\Release\ModuleLagrangianGlobal.mod"\
	".\Release\ModuleTurbGOTM.mod"\
	".\Release\ModuleWaterProperties.mod"\
	".\Release\ModuleWaves.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleJet.F90
DEP_F90_MODULEJ=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleLagrangian.F90
DEP_F90_MODULEL=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleLightExtinction.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_1\Release\ModuleTriangulation.mod"\
	"..\Mohid_Base_1\Release\ModuleWaterQuality.mod"\
	"..\Mohid_Base_2\Release\ModuleBoxDif.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	"..\Mohid_Base_2\Release\ModuleStatistic.mod"\
	".\Release\ModuleAssimilation.mod"\
	".\Release\ModuleHydrodynamic.mod"\
	".\Release\ModuleJet.mod"\
	".\Release\ModuleOil.mod"\
	".\Release\ModuleTurbulence.mod"\
	".\Release\ModuleWaterProperties.mod"\
	".\Release\ModuleWaves.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleLagrangianGlobal.f90
DEP_F90_MODULELA=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleLightExtinction.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_1\Release\ModuleTriangulation.mod"\
	"..\Mohid_Base_1\Release\ModuleWaterQuality.mod"\
	"..\Mohid_Base_2\Release\ModuleBoxDif.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	"..\Mohid_Base_2\Release\ModuleStatistic.mod"\
	".\Release\ModuleAssimilation.mod"\
	".\Release\ModuleHydrodynamic.mod"\
	".\Release\ModuleJet.mod"\
	".\Release\ModuleOil.mod"\
	".\Release\ModuleTurbulence.mod"\
	".\Release\ModuleWaterProperties.mod"\
	".\Release\ModuleWaves.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleModel.F90
DEP_F90_MODULEM=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_2\Release\ModuleAtmosphere.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	".\Release\ModuleConsolidation.mod"\
	".\Release\ModuleHydrodynamic.mod"\
	".\Release\ModuleInterfaceSedimentWater.mod"\
	".\Release\ModuleInterfaceWaterAir.mod"\
	".\Release\ModuleLagrangian.mod"\
	".\Release\ModuleLagrangianGlobal.mod"\
	".\Release\ModuleSedimentProperties.mod"\
	".\Release\ModuleTurbulence.mod"\
	".\Release\ModuleWaterProperties.mod"\
	".\Release\ModuleWaves.mod"\
	
NODEP_F90_MODULEM=\
	".\Release\ModuleSequentialAssimilation.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleOil.F90
DEP_F90_MODULEO=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleOpenBoundary.F90
DEP_F90_MODULEOP=\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTriangulation.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	".\Release\ModuleGauge.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleSand.F90
DEP_F90_MODULES=\
	"..\Mohid_Base_1\Release\ModuleDischarges.mod"\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleBoxDif.mod"\
	"..\Mohid_Base_2\Release\ModuleFillMatrix.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	".\Release\ModuleWaves.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleSedimentProperties.F90
DEP_F90_MODULESE=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleInterface.mod"\
	"..\Mohid_Base_1\Release\ModuleLUD.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleAdvectionDiffusion.mod"\
	"..\Mohid_Base_2\Release\ModuleBoxDif.mod"\
	"..\Mohid_Base_2\Release\ModuleFillMatrix.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	".\Release\ModuleConsolidation.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleSequentialAssimilation.f90
DEP_F90_MODULESEQ=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	".\Release\ModuleHydrodynamic.mod"\
	".\Release\ModuleWaterProperties.mod"\
	

!IF  "$(CFG)" == "MohidWater - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "MohidWater - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "MohidWater - Win32 Debug_MPI"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\ModuleToga.F90
DEP_F90_MODULET=\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleTurbGOTM.F90
DEP_F90_MODULETU=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	".\Release\ModuleGOTM.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleTurbulence.F90
DEP_F90_MODULETUR=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleProfile.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleFillMatrix.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	"..\Mohid_Base_2\Release\ModuleStatistic.mod"\
	".\Release\ModuleTurbGOTM.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleWaterProperties.F90
DEP_F90_MODULEW=\
	"..\Mohid_Base_1\Release\ModuleDischarges.mod"\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleHydroIntegration.mod"\
	"..\Mohid_Base_1\Release\ModuleInterface.mod"\
	"..\Mohid_Base_1\Release\ModuleLightExtinction.mod"\
	"..\Mohid_Base_1\Release\ModuleProfile.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleAdvectionDiffusion.mod"\
	"..\Mohid_Base_2\Release\ModuleBoxDif.mod"\
	"..\Mohid_Base_2\Release\ModuleFillMatrix.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	"..\Mohid_Base_2\Release\ModuleMap.mod"\
	"..\Mohid_Base_2\Release\ModuleStatistic.mod"\
	".\mpif.f90"\
	".\Release\ModuleAssimilation.mod"\
	".\Release\ModuleFreeVerticalMovement.mod"\
	".\Release\ModuleHydrodynamic.mod"\
	".\Release\ModuleTurbulence.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleWaves.F90
DEP_F90_MODULEWA=\
	"..\Mohid_Base_1\Release\ModuleDrawing.mod"\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	"..\Mohid_Base_2\Release\ModuleFillMatrix.mod"\
	"..\Mohid_Base_2\Release\ModuleGeometry.mod"\
	"..\Mohid_Base_2\Release\ModuleGridData.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalGrid.mod"\
	"..\Mohid_Base_2\Release\ModuleHorizontalMap.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
