#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <QDebug>
#include <QHostInfo>
#include <math.h>

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    ui->settingsWidget->setSettings(&mSettings);
    ui->settingsWidget->setDefaultSettings();

    mCamPlayer = 0;
    mJoystick = new Joystick(this);
    mPacketInterface = new PacketInterface(this);
    mTimer = new QTimer(this);
    mTimer->setInterval(50);
    mTimer->start();
    mStatusLabel = new QLabel(this);

    // Add car to mapWidget
    CarInfo car("The Car", Qt::blue);
    LocPoint loc(500, 500, 3.14 / 4);
    car.setLocation(loc);
    car.setColor(Qt::blue);
    ui->mapWidget->addCar(car);

    connect(mTimer, SIGNAL(timeout()), this, SLOT(timerSlot()));
    connect(mJoystick, SIGNAL(buttonPressed(int)),
            this, SLOT(joystickButtonPressed(int)));
    connect(mJoystick, SIGNAL(joystickError(int,JoystickErrorType)),
            this, SLOT(joystickError(int,JoystickErrorType)));
    connect(mPacketInterface, SIGNAL(carValuesReceived(PacketInterface::MC_VALUES)),
            this, SLOT(carValuesReceived(PacketInterface::MC_VALUES)));
    connect(mPacketInterface, SIGNAL(carPosReceived(LocPoint)),
            this, SLOT(carPosReceived(LocPoint)));
    connect(mPacketInterface, SIGNAL(carResponseTimedOut()),
            this, SLOT(carResponseTimedOut()));
    connect(mPacketInterface, SIGNAL(carSensorsUltraReceived(QVector<PacketInterface::ULTRA_SENSOR_VALUE>)),
            this, SLOT(carSensorsUltraReceived(QVector<PacketInterface::ULTRA_SENSOR_VALUE>)));
    connect(mPacketInterface, SIGNAL(carSensorsIrReceived(QVector<quint16>)),
            this, SLOT(carSensorsIrReceived(QVector<quint16>)));

    ui->statusBar->addPermanentWidget(mStatusLabel);
    setWindowTitle("CaroloCup Controller");
    on_applyButton_clicked();
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::timerSlot() {
    if (mJoystick->isConnected())
    {
        double speed = 0.0;
        signed int servo1 = 128, servo2 = 128;

        if (mSettings.joystickType == JS_WHEEL)
        {
            signed long pb = mJoystick->getAxis(2) + 32768;
            if (pb < 0)
            {
                pb = 0;
            }
            if (pb > 0xFFFF) {
                pb = 0xFFFF;
            }
            signed long pg = mJoystick->getAxis(1) + 32768;
            if (pg < 0)
            {
                pg = 0;
            }
            if (pg > 0xFFFF) {
                pg = 0xFFFF;
            }

            servo1 = (pb - pg) / 2;
            servo1 = servo1 >> 8;
            servo1 += 128;

            servo2 = mJoystick->getAxis(0) + 32768;
            if (servo2 < 0)
            {
                servo2 = 0;
            }
            if (servo2 > 0xFFFF) {
                servo2 = 0xFFFF;
            }
            servo2 = servo2 >> 8;
        } else if (mSettings.joystickType == JS_XBOX_360)
        {
            signed long pb = mJoystick->getAxis(5) + 32768;
            if (pb < 0)
            {
                pb = 0;
            }
            if (pb > 0xFFFF) {
                pb = 0xFFFF;
            }
            signed long pg = mJoystick->getAxis(2) + 32768;
            if (pg < 0)
            {
                pg = 0;
            }
            if (pg > 0xFFFF) {
                pg = 0xFFFF;
            }

            servo1 = (pb - pg) / 2;
            servo1 = servo1 >> 8;
            servo1 += 128;

            servo2 = mJoystick->getAxis(3) + 32768;
            if (servo2 < 0)
            {
                servo2 = 0;
            }
            if (servo2 > 0xFFFF) {
                servo2 = 0xFFFF;
            }
            servo2 = servo2 >> 8;
        } else if (mSettings.joystickType == JS_XBOX_360_2)
        {
            servo1 = -mJoystick->getAxis(1) + 32768;
            if (servo1 < 0)
            {
                servo1 = 0;
            }
            if (servo1 > 0xFFFF) {
                servo1 = 0xFFFF;
            }
            servo1 = servo1 >> 8;

            // Axis was 3
            servo2 = mJoystick->getAxis(3) + 32768;
            if (servo2 < 0)
            {
                servo2 = 0;
            }
            if (servo2 > 0xFFFF) {
                servo2 = 0xFFFF;
            }
            servo2 = servo2 >> 8;
        }

        servo2 += mSettings.steeringOffset;
        if (servo2 < 0) {
            servo2 = 0;
        } else if (servo2 > 0xFF) {
            servo2 = 0xFF;
        }

        // Add hysteres for the throttle command.
        if (servo1 >= 128)
        {
            servo1 -= 128;
            if (servo1 > mSettings.hysteres)
            {
                servo1 -= mSettings.hysteres;
            } else
            {
                servo1 = 0;
            }

            speed = (mSettings.topVal * servo1) / (double)(127 - mSettings.hysteres);
        } else
        {
            servo1 = 127 - servo1;
            if (servo1 > mSettings.hysteres)
            {
                servo1 -= mSettings.hysteres;
            } else
            {
                servo1 = 0;
            }

            speed = -(mSettings.topVal * servo1) / (double)(127 - mSettings.hysteres);
        }

        // Drive car from here
        if (ui->sendJoystickCheckBox->isChecked()) {
            mPacketInterface->setMotorServo(speed , 255 - servo2);
        }
    }

    // Update the battery voltages etc if the checkbox is checked
    if (ui->updateReadingskBox->isChecked()) {
        mPacketInterface->readValues();
    }

    // Update the car position on the map if the checkbox is checked
    if (ui->updatePosBox->isChecked()) {
        mPacketInterface->readPosition();
    }

    // Update sensor readings
    if (ui->sensorUpdatekBox->isChecked()) {
        mPacketInterface->readSensorsUltra();
        mPacketInterface->readSensorsIr();
    }
}

void MainWindow::joystickButtonPressed(int button) {
    qDebug() << "Joystick button pressed: " << button;
}

void MainWindow::joystickError(int error, JoystickErrorType errorType) {
    Q_UNUSED(error);
    Q_UNUSED(errorType);
}

void MainWindow::carValuesReceived(PacketInterface::MC_VALUES values)
{
    ui->vinLcd->display(values.v_batt);
    ui->vlogLcd->display(values.v_log);
    ui->tempLcd->display(values.temp);
    ui->speedLcd->display(values.speed);

    int mvIn = (int)(values.v_batt * 1000.0);
    int mvLog = (int)(values.v_log * 1000.0);

    if (mvIn >= ui->vinBar->minimum()) {
        ui->vinBar->setValue(mvIn);
    } else {
        ui->vinBar->setValue(ui->vinBar->minimum());
    }

    if (mvLog >= ui->vlogBar->minimum()) {
        ui->vlogBar->setValue(mvLog);
    } else {
        ui->vlogBar->setValue(ui->vlogBar->minimum());
    }
}

void MainWindow::carPosReceived(LocPoint pos)
{
    ui->mapWidget->getCarInfo(0)->setLocation(pos);
    ui->mapWidget->repaint();
}

void MainWindow::carResponseTimedOut()
{
    qDebug() << "Response from car timed out.";
}

void MainWindow::carSensorsUltraReceived(QVector<PacketInterface::ULTRA_SENSOR_VALUE> values)
{
    ui->sensor1Lcd->display(values[0].address);
    ui->sensor2Lcd->display(values[1].address);
    ui->sensor3Lcd->display(values[2].address);

    ui->sensor1Bar->setValue(values[0].value);
    ui->sensor2Bar->setValue(values[1].value);
    ui->sensor3Bar->setValue(values[2].value);
}

void MainWindow::carSensorsIrReceived(QVector<quint16> values)
{
     ui->sensor1Bar_ir->setValue(values[0]);
     ui->sensor2Bar_ir->setValue(values[1]);
     ui->sensor3Bar_ir->setValue(values[2]);
}

void MainWindow::on_connectJoystickButton_clicked()
{
    if (mJoystick->init(mSettings.joystickPort) == 0) {
        qDebug() << "Axes:" << mJoystick->numAxes();
        qDebug() << "Buttons:" << mJoystick->numButtons();
        qDebug() << "Name:" << mJoystick->getName();
    } else {
        qWarning() << "Opening joystick failed.";
    }
}

Phonon::MediaObject *MainWindow::getCamera(const QString &host)
{
    QString path;
    path.sprintf("http://%s:8080/?action=stream", host.toAscii().data());
    return Phonon::createPlayer(Phonon::VideoCategory,
                                Phonon::MediaSource(path));
}


void MainWindow::on_connectCameraButton_clicked()
{
    if (mCamPlayer) {
        mCamPlayer->stop();
        delete mCamPlayer;
        mCamPlayer = 0;
    }

    mCamPlayer = getCamera(ui->ipAddressEdit->text());
    Phonon::createPath(mCamPlayer, ui->videoWidget);
    mCamPlayer->play();
}

void MainWindow::on_stopCameraButton_clicked()
{
    if (mCamPlayer) {
        mCamPlayer->stop();
        delete mCamPlayer;
        mCamPlayer = 0;
    }
}

void MainWindow::on_applyButton_clicked()
{
    QHostAddress addr;
    QHostInfo info = QHostInfo::fromName(ui->ipAddressEdit->text());

    if (info.error() != QHostInfo::NoError) {
        qDebug() << "Lookup failed:" << info.errorString();
        return;
    }

    foreach (const QHostAddress &address, info.addresses()) {
        qDebug() << "Found address:" << address.toString();
        mStatusLabel->setText("Car IP Address: " + address.toString());
    }

    addr.setAddress(info.addresses()[0].toString());
    mPacketInterface->connectUdp(addr, ui->portBox->value());
//    mPacketInterface->connectUdp(QHostAddress(ui->ipAddressEdit->text()), ui->portBox->value());
}


void MainWindow::on_setPositionButton_clicked()
{
    LocPoint pos;
    pos.setXY((double)ui->xPosSpinBox->value(), (double)ui->yPosSpinBox->value());
    pos.setAlpha((double)ui->angleSpinBox->value() * M_PI / 180.0);
    mPacketInterface->setPosition(pos);
}
