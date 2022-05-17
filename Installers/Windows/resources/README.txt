
Requirements
------------

JPackage is used to create the MSI installer. 
JPackage is shipped with JDK14 and above.
https://www.oracle.com/uk/java/technologies/javase/jdk14-archive-downloads.html

WIX 3.0 or later toolset is also used to create the underlying custom installer.
https://wixtoolset.org/releases/

Setup
-----

The minima-all.jar has all the Minima classes and dependencies. This is 0.101

To append the install path to the environment System variables, the main.wxs is supplied with necessary edits.
<Feature Id="DefaultFeature" Title="!(loc.MainFeatureTitle)" Level="1">
  <ComponentGroupRef Id="Shortcuts"/>
  <ComponentGroupRef Id="Files"/>
  <ComponentGroupRef Id="FileAssociations"/>
  <Component Id="pathEnvironmentVariable" Guid="{YOUR_GUID}" KeyPath="yes" Directory="TARGETDIR">
    <Environment Id="MyPathVariable" Name="Path" Value="[INSTALLDIR]" Action="set" System="no" Permanent="no" Part="last" Separator=";" />
  </Component>
</Feature>

If you are creating a new installer, replace YOUR_GUID with the GUID generated with the generator tool https://www.guidgen.com/ 
For example: [...] Guid="{607ea423-79e0-4866-9ed7-62005b88d225}"

Creating Installer
------------------

Run to create the installer, from resources directory:

jpackage --input .   --name Minima  --main-jar minima-all.jar  --main-class org.minima.Minima --type msi --java-options '--enable-preview' --resource-dir . --icon minima.ico  --win-console --win-dir-chooser --win-menu --win-menu-group Minima --win-shortcut




