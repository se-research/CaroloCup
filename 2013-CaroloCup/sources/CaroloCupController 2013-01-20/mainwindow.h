#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <phonon>
#include <QLabel>
#include "joystick.h"
#include "packetinterface.h"
#include "settingswidget.h"
#include "carinfo.h"
#include "locpoint.h"

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT
    
public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();
    Phonon::MediaObject *getCamera(const QString &host);

private Q_SLOTS:
    void timerSlot();
    void joystickButtonPressed(int button);
    void joystickError(int error, JoystickErrorType errorType);
    void carValuesReceived(PacketInterface::MC_VALUES values);
    void carPosReceived(LocPoint pos);
    void carResponseTimedOut();
    void carSensorsUltraReceived(QVector<PacketInterface::ULTRA_SENSOR_VALUE> values);
    void carSensorsIrReceived(QVector<quint16> values);

    void on_connectJoystickButton_clicked();
    void on_connectCameraButton_clicked();
    void on_stopCameraButton_clicked();
    void on_applyButton_clicked();
    void on_setPositionButton_clicked();

private:
    Ui::MainWindow *ui;
    Joystick *mJoystick;
    PacketInterface *mPacketInterface;
    settings_t mSettings;
    QTimer *mTimer;
    Phonon::MediaObject *mCamPlayer;
    QLabel *mStatusLabel;
};

#endif // MAINWINDOW_H
