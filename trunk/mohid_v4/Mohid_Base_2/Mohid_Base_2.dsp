# Microsoft Developer Studio Project File - Name="Mohid_Base_2" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=Mohid_Base_2 - Win32 Debug_MPI
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Mohid_Base_2.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Mohid_Base_2.mak" CFG="Mohid_Base_2 - Win32 Debug_MPI"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Mohid_Base_2 - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Mohid_Base_2 - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "Mohid_Base_2 - Win32 Debug_MPI" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Mohid_Base_2 - Win32 Release"

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
# ADD F90 /architecture:pn4 /check:bounds /check:power /check:overflow /check:underflow /compile_only /debug:none /fpp /include:"../Libs" /include:"../Mohid_Base_1/Release" /nologo /stand:f95 /traceback /tune:pn4 /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused /fast
# SUBTRACT F90 /check:arg_temp_created /check:format /check:output_conversion /warn:nofileopt
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

!ELSEIF  "$(CFG)" == "Mohid_Base_2 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /architecture:pn4 /check:arg_temp_created /check:bounds /check:format /check:power /check:output_conversion /check:overflow /check:underflow /compile_only /dbglibs /debug:full /fpp /include:"../Mohid_Base_1/Debug" /nologo /reentrancy:threaded /stand:f95 /traceback /tune:pn4 /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused /include:"/Libs"
# SUBTRACT F90 /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x816 /d "_DEBUG"
# ADD RSC /l 0x816 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "Mohid_Base_2 - Win32 Debug_MPI"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Mohid_Base_2___Win32_Debug_MPI"
# PROP BASE Intermediate_Dir "Mohid_Base_2___Win32_Debug_MPI"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug_MPI"
# PROP Intermediate_Dir "Debug_MPI"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /fpp /include:"../Mohid_Base_1/Debug" /nologo /stand:f95 /traceback /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused /include:"/Libs"
# SUBTRACT BASE F90 /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /define:"_USE_MPI" /fpp /include:"../Mohid_Base_1/Debug_MPI" /nologo /stand:f95 /traceback /warn:argument_checking /warn:declarations /warn:truncated_source /warn:unused /include:"/Libs"
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x816 /d "_DEBUG"
# ADD RSC /l 0x816 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "Mohid_Base_2 - Win32 Release"
# Name "Mohid_Base_2 - Win32 Debug"
# Name "Mohid_Base_2 - Win32 Debug_MPI"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\ModuleAdvectionDiffusion.F90
DEP_F90_MODUL=\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	".\Release\ModuleGeometry.mod"\
	".\Release\ModuleHorizontalGrid.mod"\
	".\Release\ModuleHorizontalMap.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleAtmosphere.F90
DEP_F90_MODULE=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	".\Release\ModuleFillMatrix.mod"\
	".\Release\ModuleGridData.mod"\
	".\Release\ModuleHorizontalGrid.mod"\
	".\Release\ModuleStatistic.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleBasinGeometry.F90
DEP_F90_MODULEB=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	".\Release\ModuleGridData.mod"\
	".\Release\ModuleHorizontalGrid.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleBoxDif.F90
DEP_F90_MODULEBO=\
	"..\Mohid_Base_1\Release\ModuleDrawing.mod"\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	".\Release\ModuleGridData.mod"\
	".\Release\ModuleHorizontalGrid.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleFillMatrix.F90
DEP_F90_MODULEF=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTimeSerie.mod"\
	".\Release\ModuleBoxDif.mod"\
	".\Release\ModuleGeometry.mod"\
	".\Release\ModuleGridData.mod"\
	".\Release\ModuleHorizontalGrid.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleGeometry.F90
DEP_F90_MODULEG=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	".\Release\ModuleGridData.mod"\
	".\Release\ModuleHorizontalGrid.mod"\
	".\Release\ModuleHorizontalMap.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleGridData.F90
DEP_F90_MODULEGR=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	".\Release\ModuleHorizontalGrid.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleHorizontalGrid.F90
DEP_F90_MODULEH=\
	"..\Mohid_Base_1\Release\ModuleDrawing.mod"\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	
NODEP_F90_MODULEH=\
	".\Release\proj4.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleHorizontalMap.F90
DEP_F90_MODULEHO=\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	".\Release\ModuleGridData.mod"\
	".\Release\ModuleHorizontalGrid.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleInterpolation.F90
DEP_F90_MODULEI=\
	"..\Mohid_Base_1\Release\ModuleDrawing.mod"\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	"..\Mohid_Base_1\Release\ModuleTriangulation.mod"\
	".\Release\ModuleGeometry.mod"\
	".\Release\ModuleHorizontalGrid.mod"\
	".\Release\ModuleHorizontalMap.mod"\
	".\Release\ModuleMap.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleMap.F90
DEP_F90_MODULEM=\
	"..\Mohid_Base_1\Release\ModuleFunctions.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	".\Release\ModuleGeometry.mod"\
	".\Release\ModuleGridData.mod"\
	".\Release\ModuleHorizontalGrid.mod"\
	".\Release\ModuleHorizontalMap.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ModuleStatistic.F90
DEP_F90_MODULES=\
	"..\Mohid_Base_1\Release\ModuleEnterData.mod"\
	"..\Mohid_Base_1\Release\ModuleGlobalData.mod"\
	"..\Mohid_Base_1\Release\ModuleHDF5.mod"\
	"..\Mohid_Base_1\Release\ModuleStopWatch.mod"\
	"..\Mohid_Base_1\Release\ModuleTime.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# End Target
# End Project
