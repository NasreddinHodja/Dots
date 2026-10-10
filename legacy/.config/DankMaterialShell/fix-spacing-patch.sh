#!/bin/bash
set -e

# Patch 1: DankBarWindow.qml — add separate spacingX/spacingY support
TARGET="/usr/share/quickshell/dms/Modules/DankBar/DankBarWindow.qml"

sudo python3 -c "
path = '$TARGET'
with open(path) as f:
    content = f.read()

old = '''                    property int spacingPx: Theme.px(barWindow.effectiveSpacing, barWindow._dpr)
                    anchors.fill: parent
                    anchors.leftMargin: !barWindow.isVertical ? spacingPx : (axis.edge === \"left\" ? spacingPx : 0)
                    anchors.rightMargin: !barWindow.isVertical ? spacingPx : (axis.edge === \"right\" ? spacingPx : 0)
                    anchors.topMargin: barWindow.isVertical ? (barWindow.hasAdjacentTopBar ? 0 : spacingPx) : (axis.outerVisualEdge() === \"bottom\" ? 0 : spacingPx)
                    anchors.bottomMargin: barWindow.isVertical ? (barWindow.hasAdjacentBottomBar ? 0 : spacingPx) : (axis.outerVisualEdge() === \"bottom\" ? spacingPx : 0)'''

new = '''                    property int spacingPx: Theme.px(barWindow.effectiveSpacing, barWindow._dpr)
                    property int spacingXPx: barConfig?.spacingX !== undefined ? Theme.px(barConfig.spacingX, barWindow._dpr) : spacingPx
                    property int spacingYPx: barConfig?.spacingY !== undefined ? Theme.px(barConfig.spacingY, barWindow._dpr) : spacingPx
                    anchors.fill: parent
                    anchors.leftMargin: !barWindow.isVertical ? spacingXPx : (axis.edge === \"left\" ? spacingXPx : 0)
                    anchors.rightMargin: !barWindow.isVertical ? spacingXPx : (axis.edge === \"right\" ? spacingXPx : 0)
                    anchors.topMargin: barWindow.isVertical ? (barWindow.hasAdjacentTopBar ? 0 : spacingYPx) : (axis.outerVisualEdge() === \"bottom\" ? 0 : spacingYPx)
                    anchors.bottomMargin: barWindow.isVertical ? (barWindow.hasAdjacentBottomBar ? 0 : spacingYPx) : (axis.outerVisualEdge() === \"bottom\" ? spacingYPx : 0)'''

if new.strip() in content:
    print('Patch already applied, nothing to do.')
    exit(0)

assert old in content, 'Pattern not found — DMS may have updated again, patch needs review.'
with open(path, 'w') as f:
    f.write(content.replace(old, new, 1))
print('Patch applied successfully.')
"

# Patch 2: CenterSection.qml — vertically center wrapper items so spacingY works
TARGET2="/usr/share/quickshell/dms/Modules/DankBar/CenterSection.qml"

sudo python3 -c "
path = '$TARGET2'
with open(path) as f:
    content = f.read()

old = '''            height: widgetLoader.item ? widgetLoader.item.height : 0

            readonly property bool active: widgetLoader.active'''

new = '''            height: widgetLoader.item ? widgetLoader.item.height : 0
            anchors.verticalCenter: !root.isVertical ? root.verticalCenter : undefined

            readonly property bool active: widgetLoader.active'''

if new.strip() in content:
    print('Patch 2 already applied, nothing to do.')
    exit(0)

assert old in content, 'Pattern not found in CenterSection.qml — DMS may have updated, patch needs review.'
with open(path, 'w') as f:
    f.write(content.replace(old, new, 1))
print('Patch 2 applied successfully.')
"
