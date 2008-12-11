# Microsoft Developer Studio Project File - Name="Mohid_Base_1" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=Mohid_Base_1 - Win32 Debug_MPI
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Mohid_Base_1.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Mohid_Base_1.mak" CFG="Mohid_Base_1 - Win32 Debug_MPI"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Mohid_Base_1 - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Mohid_Base_1 - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "Mohid_Base_1 - Win32 Debug_MPI" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Mohid_Base_1 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /architecture:pn4 /check:bounds /check:power /check:overflow /check:underflow /compile_only /debug:none /fpp /include:"../Libs" /nologo /stand:f95 /traceback /tune:pn4 /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused /fast
# SUBTRACT F90 /check:arg_temp_created /check:format /check:output_conversion /fpscomp:symbols /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x816 /d "NDEBUG"
# ADD RSC /l 0x816 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "Mohid_Base_1 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Mohid_Base_1___Win32_Debug"
# PROP BASE Intermediate_Dir "Mohid_Base_1___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Mohid_Base_1___Win32_Debug"
# PROP Intermediate_Dir "Mohid_Base_1___Win32_Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /architecture:pn4 /check:arg_temp_created /check:bounds /check:format /check:power /check:output_conversion /check:overflow /check:underflow /compile_only /dbglibs /debug:full /fpp /include:"../Libs" /nologo /reentrancy:threaded /stand:f95 /traceback /tune:pn4 /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB"
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x816 /d "_DEBUG"
# ADD RSC /l 0x816 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"Debug\Mohid_Base_1.lib"

!ELSEIF  "$(CFG)" == "Mohid_Base_1 - Win32 Debug_MPI"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Mohid_Base_1___Win32_Debug_MPI"
# PROP BASE Intermediate_Dir "Mohid_Base_1___Win32_Debug_MPI"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug_MPI"
# PROP Intermediate_Dir "Debug_MPI"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /fpp /include:"../Libs" /nologo /stand:f95 /traceback /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB"
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /define:"_USE_MPI" /fpp /include:"../Libs" /nologo /stand:f95 /traceback /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x816 /d "_DEBUG"
# ADD RSC /l 0x816 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo /out:"Debug\Mohid_Base_1.lib"
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "Mohid_Base_1 - Win32 Release"
# Name "Mohid_Base_1 - Win32 Debug"
# Name "Mohid_Base_1 - Win32 Debug_MPI"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\ModuleBenthos.F90
DEP_F90_MODUL=\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleGlobalData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleCEQUALW2.f90
DEP_F90_MODULE=\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleFunctions.mod"\
	".\Release\ModuleGlobalData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleDischarges.F90
DEP_F90_MODULED=\
	".\Release\ModuleDrawing.mod"\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleFunctions.mod"\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleTime.mod"\
	".\Release\ModuleTimeSerie.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleDrainageNetwork.F90
DEP_F90_MODULEDR=\
	".\Release\ModuleDischarges.mod"\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleFunctions.mod"\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleHDF5.mod"\
	".\Release\ModuleInterface.mod"\
	".\Release\ModuleLightExtinction.mod"\
	".\Release\ModuleStopWatch.mod"\
	".\Release\ModuleTime.mod"\
	".\Release\ModuleTimeSerie.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleDrawing.F90
DEP_F90_MODULEDRA=\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleFunctions.mod"\
	".\Release\ModuleGlobalData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleEnterData.F90
DEP_F90_MODULEE=\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleTime.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleFunctions.F90
DEP_F90_MODULEF=\
	".\mpif.f90"\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleTime.mod"\
	
NODEP_F90_MODULEF=\
	".\Release\proj4.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleGlobalData.F90
# End Source File
# Begin Source File

SOURCE=.\ModuleHDF5.F90
DEP_F90_MODULEH=\
	"..\Libs\HDF5.mod"\
	".\Release\ModuleGlobalData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleHydroIntegration.F90
DEP_F90_MODULEHY=\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleTime.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleInterface.F90
DEP_F90_MODULEI=\
	".\Release\ModuleBenthos.mod"\
	".\Release\ModuleCEQUALW2.mod"\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleLife.mod"\
	".\Release\ModuleMacroAlgae.mod"\
	".\Release\ModuleSedimentQuality.mod"\
	".\Release\ModuleTime.mod"\
	".\Release\ModuleWaterQuality.mod"\
	
NODEP_F90_MODULEI=\
	".\Release\ModuleBFM.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleLife.F90
DEP_F90_MODULEL=\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleFunctions.mod"\
	".\Release\ModuleGlobalData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleLightExtinction.F90
DEP_F90_MODULELI=\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleFunctions.mod"\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleTime.mod"\
	".\Release\ModuleTimeSerie.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleLUD.F90
DEP_F90_MODULELU=\
	".\Release\ModuleGlobalData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleMacroAlgae.f90
DEP_F90_MODULEM=\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleFunctions.mod"\
	".\Release\ModuleGlobalData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleProfile.F90
DEP_F90_MODULEP=\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleHDF5.mod"\
	".\Release\ModuleTime.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleSedimentQuality.F90
DEP_F90_MODULES=\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleFunctions.mod"\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleLUD.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleStopWatch.F90
DEP_F90_MODULEST=\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleTime.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleTime.F90
DEP_F90_MODULET=\
	".\Release\ModuleGlobalData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleTimeSerie.F90
DEP_F90_MODULETI=\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleTime.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleTriangulation.F90
DEP_F90_MODULETR=\
	".\Release\ModuleGlobalData.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleWaterQuality.F90
DEP_F90_MODULEW=\
	".\Release\ModuleEnterData.mod"\
	".\Release\ModuleFunctions.mod"\
	".\Release\ModuleGlobalData.mod"\
	".\Release\ModuleLUD.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# End Target
# End Project
