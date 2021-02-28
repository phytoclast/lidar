<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis minScale="1e+08" hasScaleBasedVisibilityFlag="0" styleCategories="AllStyleCategories" maxScale="0" version="3.16.3-Hannover">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
  </flags>
  <temporal mode="0" fetchMode="0" enabled="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <property key="WMSBackgroundLayer" value="false"/>
    <property key="WMSPublishDataSourceUrl" value="false"/>
    <property key="embeddedWidgets/count" value="0"/>
    <property key="identify/format" value="Value"/>
  </customproperties>
  <pipe>
    <provider>
      <resampling zoomedOutResamplingMethod="nearestNeighbour" maxOversampling="2" enabled="false" zoomedInResamplingMethod="nearestNeighbour"/>
    </provider>
    <rasterrenderer type="singlebandpseudocolor" classificationMin="0" alphaBand="-1" nodataColor="" band="1" opacity="1" classificationMax="93.574">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>MinMax</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <rastershader>
        <colorrampshader labelPrecision="4" clip="0" minimumValue="0" classificationMode="1" colorRampType="DISCRETE" maximumValue="93.574">
          <colorramp type="gradient" name="[source]">
            <prop k="color1" v="247,252,245,255"/>
            <prop k="color2" v="0,68,27,255"/>
            <prop k="discrete" v="0"/>
            <prop k="rampType" v="gradient"/>
            <prop k="stops" v="0.13;229,245,224,255:0.26;199,233,192,255:0.39;161,217,155,255:0.52;116,196,118,255:0.65;65,171,93,255:0.78;35,139,69,255:0.9;0,109,44,255"/>
          </colorramp>
          <item alpha="255" value="2" color="#f9f9f9" label="&lt;= 2.0000"/>
          <item alpha="255" value="5" color="#ffd6a6" label="2.0000 - 5.0000"/>
          <item alpha="255" value="15" color="#6fff21" label="5.0000 - 15.0000"/>
          <item alpha="255" value="30" color="#3dcc30" label="15.0000 - 30.0000"/>
          <item alpha="255" value="45" color="#328e4e" label="30.0000 - 45.0000"/>
          <item alpha="255" value="60" color="#2acae2" label="45.0000 - 60.0000"/>
          <item alpha="255" value="75" color="#086083" label="60.0000 - 75.0000"/>
          <item alpha="255" value="inf" color="#d937b9" label="> 75.0000"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast gamma="1" brightness="0" contrast="0"/>
    <huesaturation grayscaleMode="0" colorizeStrength="100" saturation="0" colorizeBlue="128" colorizeRed="255" colorizeOn="0" colorizeGreen="128"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
