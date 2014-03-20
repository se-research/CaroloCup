/****************************************************************************
** Meta object code from reading C++ file 'packetinterface.h'
**
** Created: Thu Nov 29 10:08:54 2012
**      by: The Qt Meta Object Compiler version 62 (Qt 4.6.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "packetinterface.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'packetinterface.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_PacketInterface[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       7,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       5,       // signalCount

 // signals: signature, parameters, type, tag, flags
      24,   17,   16,   16, 0x05,
      74,   70,   16,   16, 0x05,
      99,   16,   16,   16, 0x05,
     117,   16,   16,   16, 0x05,
     139,   17,   16,   16, 0x05,

 // slots: signature, parameters, type, tag, flags
     209,   16,   16,   16, 0x0a,
     232,   16,   16,   16, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_PacketInterface[] = {
    "PacketInterface\0\0values\0"
    "carValuesReceived(PacketInterface::MC_VALUES)\0"
    "pos\0carPosReceived(LocPoint)\0"
    "carPingReceived()\0carResponseTimedOut()\0"
    "carSensorsUltraReceived(QVector<PacketInterface::ULTRA_SENSOR_VALUE>)\0"
    "readPendingDatagrams()\0timerSlot()\0"
};

const QMetaObject PacketInterface::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_PacketInterface,
      qt_meta_data_PacketInterface, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &PacketInterface::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *PacketInterface::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *PacketInterface::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_PacketInterface))
        return static_cast<void*>(const_cast< PacketInterface*>(this));
    return QObject::qt_metacast(_clname);
}

int PacketInterface::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: carValuesReceived((*reinterpret_cast< PacketInterface::MC_VALUES(*)>(_a[1]))); break;
        case 1: carPosReceived((*reinterpret_cast< LocPoint(*)>(_a[1]))); break;
        case 2: carPingReceived(); break;
        case 3: carResponseTimedOut(); break;
        case 4: carSensorsUltraReceived((*reinterpret_cast< QVector<PacketInterface::ULTRA_SENSOR_VALUE>(*)>(_a[1]))); break;
        case 5: readPendingDatagrams(); break;
        case 6: timerSlot(); break;
        default: ;
        }
        _id -= 7;
    }
    return _id;
}

// SIGNAL 0
void PacketInterface::carValuesReceived(PacketInterface::MC_VALUES _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void PacketInterface::carPosReceived(LocPoint _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void PacketInterface::carPingReceived()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void PacketInterface::carResponseTimedOut()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}

// SIGNAL 4
void PacketInterface::carSensorsUltraReceived(QVector<PacketInterface::ULTRA_SENSOR_VALUE> _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}
QT_END_MOC_NAMESPACE
