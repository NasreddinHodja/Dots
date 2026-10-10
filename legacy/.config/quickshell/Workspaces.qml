import QtQuick

// Tags active on the other monitor are dimly highlighted (full highlight
// if active on both monitors).
WorkspacesView {
    tags: Mango.tags
    monitorName: Mango.monitorName
    otherActiveTagIndices: Mango.otherActiveTagIndices
}
