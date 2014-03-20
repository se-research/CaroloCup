/********************************************************************************
** Form generated from reading UI file 'mainwindow.ui'
**
** Created: Thu Jan 24 19:28:08 2013
**      by: Qt User Interface Compiler version 4.7.4
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWINDOW_H
#define UI_MAINWINDOW_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLCDNumber>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QMainWindow>
#include <QtGui/QMenuBar>
#include <QtGui/QProgressBar>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QSpinBox>
#include <QtGui/QStatusBar>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include <phonon/videowidget.h>
#include "mapwidget.h"
#include "settingswidget.h"

QT_BEGIN_NAMESPACE

class Ui_MainWindow
{
public:
    QWidget *centralWidget;
    QHBoxLayout *horizontalLayout;
    QTabWidget *tabWidget;
    QWidget *tab;
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout_3;
    QLabel *label;
    QLineEdit *ipAddressEdit;
    QLabel *label_2;
    QSpinBox *portBox;
    QPushButton *applyButton;
    QCheckBox *sendJoystickCheckBox;
    Phonon::VideoWidget *videoWidget;
    QHBoxLayout *horizontalLayout_4;
    QPushButton *connectJoystickButton;
    QSpacerItem *horizontalSpacer;
    QPushButton *stopCameraButton;
    QPushButton *connectCameraButton;
    QWidget *tab_3;
    QVBoxLayout *verticalLayout_2;
    QGridLayout *gridLayout;
    QLabel *label_3;
    QLCDNumber *vinLcd;
    QLabel *label_4;
    QLCDNumber *vlogLcd;
    QLabel *label_5;
    QLCDNumber *tempLcd;
    QLabel *label_7;
    QLCDNumber *speedLcd;
    QSpacerItem *verticalSpacer;
    QGridLayout *gridLayout_2;
    QLabel *label_8;
    QProgressBar *vinBar;
    QLabel *label_9;
    QProgressBar *vlogBar;
    QHBoxLayout *horizontalLayout_5;
    QSpacerItem *horizontalSpacer_2;
    QCheckBox *updateReadingskBox;
    QWidget *tab_4;
    QVBoxLayout *verticalLayout_3;
    QHBoxLayout *horizontalLayout_6;
    QLabel *label_6;
    QSpinBox *xPosSpinBox;
    QLabel *label_10;
    QSpinBox *yPosSpinBox;
    QLabel *label_11;
    QSpinBox *angleSpinBox;
    QSpacerItem *horizontalSpacer_3;
    QPushButton *setPositionButton;
    MapWidget *mapWidget;
    QHBoxLayout *horizontalLayout_7;
    QSpacerItem *horizontalSpacer_4;
    QCheckBox *updatePosBox;
    QWidget *tab_5;
    QVBoxLayout *verticalLayout_4;
    QGridLayout *gridLayout_3;
    QLabel *label_12;
    QLCDNumber *sensor1Lcd;
    QProgressBar *sensor1Bar;
    QLabel *label_13;
    QLCDNumber *sensor2Lcd;
    QProgressBar *sensor2Bar;
    QLabel *label_14;
    QLCDNumber *sensor3Lcd;
    QProgressBar *sensor3Bar;
    QSpacerItem *verticalSpacer_2;
    QGridLayout *gridLayout_4;
    QLabel *label_15;
    QProgressBar *sensor1Bar_ir;
    QLabel *label_16;
    QProgressBar *sensor2Bar_ir;
    QLabel *label_17;
    QProgressBar *sensor3Bar_ir;
    QHBoxLayout *horizontalLayout_8;
    QSpacerItem *horizontalSpacer_5;
    QCheckBox *sensorUpdatekBox;
    QWidget *tab_2;
    QHBoxLayout *horizontalLayout_2;
    SettingsWidget *settingsWidget;
    QMenuBar *menuBar;
    QStatusBar *statusBar;

    void setupUi(QMainWindow *MainWindow)
    {
        if (MainWindow->objectName().isEmpty())
            MainWindow->setObjectName(QString::fromUtf8("MainWindow"));
        MainWindow->resize(684, 635);
        centralWidget = new QWidget(MainWindow);
        centralWidget->setObjectName(QString::fromUtf8("centralWidget"));
        horizontalLayout = new QHBoxLayout(centralWidget);
        horizontalLayout->setSpacing(6);
        horizontalLayout->setContentsMargins(11, 11, 11, 11);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        tabWidget = new QTabWidget(centralWidget);
        tabWidget->setObjectName(QString::fromUtf8("tabWidget"));
        tab = new QWidget();
        tab->setObjectName(QString::fromUtf8("tab"));
        verticalLayout = new QVBoxLayout(tab);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(6);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        label = new QLabel(tab);
        label->setObjectName(QString::fromUtf8("label"));

        horizontalLayout_3->addWidget(label);

        ipAddressEdit = new QLineEdit(tab);
        ipAddressEdit->setObjectName(QString::fromUtf8("ipAddressEdit"));

        horizontalLayout_3->addWidget(ipAddressEdit);

        label_2 = new QLabel(tab);
        label_2->setObjectName(QString::fromUtf8("label_2"));

        horizontalLayout_3->addWidget(label_2);

        portBox = new QSpinBox(tab);
        portBox->setObjectName(QString::fromUtf8("portBox"));
        portBox->setMaximum(65535);
        portBox->setValue(27800);

        horizontalLayout_3->addWidget(portBox);

        applyButton = new QPushButton(tab);
        applyButton->setObjectName(QString::fromUtf8("applyButton"));

        horizontalLayout_3->addWidget(applyButton);

        sendJoystickCheckBox = new QCheckBox(tab);
        sendJoystickCheckBox->setObjectName(QString::fromUtf8("sendJoystickCheckBox"));

        horizontalLayout_3->addWidget(sendJoystickCheckBox);


        verticalLayout->addLayout(horizontalLayout_3);

        videoWidget = new Phonon::VideoWidget(tab);
        videoWidget->setObjectName(QString::fromUtf8("videoWidget"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(videoWidget->sizePolicy().hasHeightForWidth());
        videoWidget->setSizePolicy(sizePolicy);

        verticalLayout->addWidget(videoWidget);

        horizontalLayout_4 = new QHBoxLayout();
        horizontalLayout_4->setSpacing(6);
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        connectJoystickButton = new QPushButton(tab);
        connectJoystickButton->setObjectName(QString::fromUtf8("connectJoystickButton"));

        horizontalLayout_4->addWidget(connectJoystickButton);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_4->addItem(horizontalSpacer);

        stopCameraButton = new QPushButton(tab);
        stopCameraButton->setObjectName(QString::fromUtf8("stopCameraButton"));

        horizontalLayout_4->addWidget(stopCameraButton);

        connectCameraButton = new QPushButton(tab);
        connectCameraButton->setObjectName(QString::fromUtf8("connectCameraButton"));

        horizontalLayout_4->addWidget(connectCameraButton);


        verticalLayout->addLayout(horizontalLayout_4);

        tabWidget->addTab(tab, QString());
        tab_3 = new QWidget();
        tab_3->setObjectName(QString::fromUtf8("tab_3"));
        verticalLayout_2 = new QVBoxLayout(tab_3);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setContentsMargins(11, 11, 11, 11);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        label_3 = new QLabel(tab_3);
        label_3->setObjectName(QString::fromUtf8("label_3"));

        gridLayout->addWidget(label_3, 0, 0, 1, 1);

        vinLcd = new QLCDNumber(tab_3);
        vinLcd->setObjectName(QString::fromUtf8("vinLcd"));
        vinLcd->setMinimumSize(QSize(0, 30));
        vinLcd->setNumDigits(7);
        vinLcd->setDigitCount(7);
        vinLcd->setSegmentStyle(QLCDNumber::Filled);
        vinLcd->setProperty("value", QVariant(0));

        gridLayout->addWidget(vinLcd, 0, 1, 1, 1);

        label_4 = new QLabel(tab_3);
        label_4->setObjectName(QString::fromUtf8("label_4"));

        gridLayout->addWidget(label_4, 1, 0, 1, 1);

        vlogLcd = new QLCDNumber(tab_3);
        vlogLcd->setObjectName(QString::fromUtf8("vlogLcd"));
        vlogLcd->setMinimumSize(QSize(0, 30));
        vlogLcd->setNumDigits(7);

        gridLayout->addWidget(vlogLcd, 1, 1, 1, 1);

        label_5 = new QLabel(tab_3);
        label_5->setObjectName(QString::fromUtf8("label_5"));

        gridLayout->addWidget(label_5, 2, 0, 1, 1);

        tempLcd = new QLCDNumber(tab_3);
        tempLcd->setObjectName(QString::fromUtf8("tempLcd"));
        tempLcd->setMinimumSize(QSize(0, 30));
        tempLcd->setNumDigits(7);

        gridLayout->addWidget(tempLcd, 2, 1, 1, 1);

        label_7 = new QLabel(tab_3);
        label_7->setObjectName(QString::fromUtf8("label_7"));

        gridLayout->addWidget(label_7, 3, 0, 1, 1);

        speedLcd = new QLCDNumber(tab_3);
        speedLcd->setObjectName(QString::fromUtf8("speedLcd"));
        speedLcd->setMinimumSize(QSize(0, 30));
        speedLcd->setNumDigits(7);

        gridLayout->addWidget(speedLcd, 3, 1, 1, 1);


        verticalLayout_2->addLayout(gridLayout);

        verticalSpacer = new QSpacerItem(20, 256, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer);

        gridLayout_2 = new QGridLayout();
        gridLayout_2->setSpacing(6);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        label_8 = new QLabel(tab_3);
        label_8->setObjectName(QString::fromUtf8("label_8"));

        gridLayout_2->addWidget(label_8, 0, 0, 1, 1);

        vinBar = new QProgressBar(tab_3);
        vinBar->setObjectName(QString::fromUtf8("vinBar"));
        vinBar->setMinimum(12000);
        vinBar->setMaximum(16800);
        vinBar->setValue(12000);

        gridLayout_2->addWidget(vinBar, 0, 1, 1, 1);

        label_9 = new QLabel(tab_3);
        label_9->setObjectName(QString::fromUtf8("label_9"));

        gridLayout_2->addWidget(label_9, 1, 0, 1, 1);

        vlogBar = new QProgressBar(tab_3);
        vlogBar->setObjectName(QString::fromUtf8("vlogBar"));
        vlogBar->setMinimum(6000);
        vlogBar->setMaximum(8400);
        vlogBar->setValue(6000);

        gridLayout_2->addWidget(vlogBar, 1, 1, 1, 1);


        verticalLayout_2->addLayout(gridLayout_2);

        horizontalLayout_5 = new QHBoxLayout();
        horizontalLayout_5->setSpacing(6);
        horizontalLayout_5->setObjectName(QString::fromUtf8("horizontalLayout_5"));
        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_5->addItem(horizontalSpacer_2);

        updateReadingskBox = new QCheckBox(tab_3);
        updateReadingskBox->setObjectName(QString::fromUtf8("updateReadingskBox"));

        horizontalLayout_5->addWidget(updateReadingskBox);


        verticalLayout_2->addLayout(horizontalLayout_5);

        tabWidget->addTab(tab_3, QString());
        tab_4 = new QWidget();
        tab_4->setObjectName(QString::fromUtf8("tab_4"));
        verticalLayout_3 = new QVBoxLayout(tab_4);
        verticalLayout_3->setSpacing(6);
        verticalLayout_3->setContentsMargins(11, 11, 11, 11);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        horizontalLayout_6 = new QHBoxLayout();
        horizontalLayout_6->setSpacing(6);
        horizontalLayout_6->setObjectName(QString::fromUtf8("horizontalLayout_6"));
        label_6 = new QLabel(tab_4);
        label_6->setObjectName(QString::fromUtf8("label_6"));

        horizontalLayout_6->addWidget(label_6);

        xPosSpinBox = new QSpinBox(tab_4);
        xPosSpinBox->setObjectName(QString::fromUtf8("xPosSpinBox"));
        xPosSpinBox->setMinimum(-10000);
        xPosSpinBox->setMaximum(10000);

        horizontalLayout_6->addWidget(xPosSpinBox);

        label_10 = new QLabel(tab_4);
        label_10->setObjectName(QString::fromUtf8("label_10"));

        horizontalLayout_6->addWidget(label_10);

        yPosSpinBox = new QSpinBox(tab_4);
        yPosSpinBox->setObjectName(QString::fromUtf8("yPosSpinBox"));
        yPosSpinBox->setMinimum(-10000);
        yPosSpinBox->setMaximum(10000);
        yPosSpinBox->setValue(0);

        horizontalLayout_6->addWidget(yPosSpinBox);

        label_11 = new QLabel(tab_4);
        label_11->setObjectName(QString::fromUtf8("label_11"));

        horizontalLayout_6->addWidget(label_11);

        angleSpinBox = new QSpinBox(tab_4);
        angleSpinBox->setObjectName(QString::fromUtf8("angleSpinBox"));
        angleSpinBox->setMaximum(359);

        horizontalLayout_6->addWidget(angleSpinBox);

        horizontalSpacer_3 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_6->addItem(horizontalSpacer_3);

        setPositionButton = new QPushButton(tab_4);
        setPositionButton->setObjectName(QString::fromUtf8("setPositionButton"));

        horizontalLayout_6->addWidget(setPositionButton);


        verticalLayout_3->addLayout(horizontalLayout_6);

        mapWidget = new MapWidget(tab_4);
        mapWidget->setObjectName(QString::fromUtf8("mapWidget"));
        sizePolicy.setHeightForWidth(mapWidget->sizePolicy().hasHeightForWidth());
        mapWidget->setSizePolicy(sizePolicy);

        verticalLayout_3->addWidget(mapWidget);

        horizontalLayout_7 = new QHBoxLayout();
        horizontalLayout_7->setSpacing(6);
        horizontalLayout_7->setObjectName(QString::fromUtf8("horizontalLayout_7"));
        horizontalSpacer_4 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_7->addItem(horizontalSpacer_4);

        updatePosBox = new QCheckBox(tab_4);
        updatePosBox->setObjectName(QString::fromUtf8("updatePosBox"));

        horizontalLayout_7->addWidget(updatePosBox);


        verticalLayout_3->addLayout(horizontalLayout_7);

        tabWidget->addTab(tab_4, QString());
        tab_5 = new QWidget();
        tab_5->setObjectName(QString::fromUtf8("tab_5"));
        verticalLayout_4 = new QVBoxLayout(tab_5);
        verticalLayout_4->setSpacing(6);
        verticalLayout_4->setContentsMargins(11, 11, 11, 11);
        verticalLayout_4->setObjectName(QString::fromUtf8("verticalLayout_4"));
        gridLayout_3 = new QGridLayout();
        gridLayout_3->setSpacing(6);
        gridLayout_3->setObjectName(QString::fromUtf8("gridLayout_3"));
        label_12 = new QLabel(tab_5);
        label_12->setObjectName(QString::fromUtf8("label_12"));

        gridLayout_3->addWidget(label_12, 0, 0, 1, 1);

        sensor1Lcd = new QLCDNumber(tab_5);
        sensor1Lcd->setObjectName(QString::fromUtf8("sensor1Lcd"));
        sensor1Lcd->setMinimumSize(QSize(0, 25));
        sensor1Lcd->setBaseSize(QSize(0, 0));
        sensor1Lcd->setMode(QLCDNumber::Hex);

        gridLayout_3->addWidget(sensor1Lcd, 0, 1, 1, 1);

        sensor1Bar = new QProgressBar(tab_5);
        sensor1Bar->setObjectName(QString::fromUtf8("sensor1Bar"));
        sensor1Bar->setMaximum(350);
        sensor1Bar->setValue(24);

        gridLayout_3->addWidget(sensor1Bar, 0, 2, 1, 1);

        label_13 = new QLabel(tab_5);
        label_13->setObjectName(QString::fromUtf8("label_13"));

        gridLayout_3->addWidget(label_13, 1, 0, 1, 1);

        sensor2Lcd = new QLCDNumber(tab_5);
        sensor2Lcd->setObjectName(QString::fromUtf8("sensor2Lcd"));
        sensor2Lcd->setMinimumSize(QSize(0, 25));
        sensor2Lcd->setMode(QLCDNumber::Hex);

        gridLayout_3->addWidget(sensor2Lcd, 1, 1, 1, 1);

        sensor2Bar = new QProgressBar(tab_5);
        sensor2Bar->setObjectName(QString::fromUtf8("sensor2Bar"));
        sensor2Bar->setMaximum(350);
        sensor2Bar->setValue(24);

        gridLayout_3->addWidget(sensor2Bar, 1, 2, 1, 1);

        label_14 = new QLabel(tab_5);
        label_14->setObjectName(QString::fromUtf8("label_14"));

        gridLayout_3->addWidget(label_14, 2, 0, 1, 1);

        sensor3Lcd = new QLCDNumber(tab_5);
        sensor3Lcd->setObjectName(QString::fromUtf8("sensor3Lcd"));
        sensor3Lcd->setMinimumSize(QSize(0, 25));
        sensor3Lcd->setMode(QLCDNumber::Hex);

        gridLayout_3->addWidget(sensor3Lcd, 2, 1, 1, 1);

        sensor3Bar = new QProgressBar(tab_5);
        sensor3Bar->setObjectName(QString::fromUtf8("sensor3Bar"));
        sensor3Bar->setMaximum(350);
        sensor3Bar->setValue(24);

        gridLayout_3->addWidget(sensor3Bar, 2, 2, 1, 1);


        verticalLayout_4->addLayout(gridLayout_3);

        verticalSpacer_2 = new QSpacerItem(20, 425, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_4->addItem(verticalSpacer_2);

        gridLayout_4 = new QGridLayout();
        gridLayout_4->setSpacing(6);
        gridLayout_4->setObjectName(QString::fromUtf8("gridLayout_4"));
        label_15 = new QLabel(tab_5);
        label_15->setObjectName(QString::fromUtf8("label_15"));

        gridLayout_4->addWidget(label_15, 0, 0, 1, 1);

        sensor1Bar_ir = new QProgressBar(tab_5);
        sensor1Bar_ir->setObjectName(QString::fromUtf8("sensor1Bar_ir"));
        sensor1Bar_ir->setMaximum(1023);
        sensor1Bar_ir->setValue(24);

        gridLayout_4->addWidget(sensor1Bar_ir, 0, 1, 1, 1);

        label_16 = new QLabel(tab_5);
        label_16->setObjectName(QString::fromUtf8("label_16"));

        gridLayout_4->addWidget(label_16, 1, 0, 1, 1);

        sensor2Bar_ir = new QProgressBar(tab_5);
        sensor2Bar_ir->setObjectName(QString::fromUtf8("sensor2Bar_ir"));
        sensor2Bar_ir->setMaximum(1023);
        sensor2Bar_ir->setValue(24);

        gridLayout_4->addWidget(sensor2Bar_ir, 1, 1, 1, 1);

        label_17 = new QLabel(tab_5);
        label_17->setObjectName(QString::fromUtf8("label_17"));

        gridLayout_4->addWidget(label_17, 2, 0, 1, 1);

        sensor3Bar_ir = new QProgressBar(tab_5);
        sensor3Bar_ir->setObjectName(QString::fromUtf8("sensor3Bar_ir"));
        sensor3Bar_ir->setMaximum(1023);
        sensor3Bar_ir->setValue(24);

        gridLayout_4->addWidget(sensor3Bar_ir, 2, 1, 1, 1);


        verticalLayout_4->addLayout(gridLayout_4);

        horizontalLayout_8 = new QHBoxLayout();
        horizontalLayout_8->setSpacing(6);
        horizontalLayout_8->setObjectName(QString::fromUtf8("horizontalLayout_8"));
        horizontalSpacer_5 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_8->addItem(horizontalSpacer_5);

        sensorUpdatekBox = new QCheckBox(tab_5);
        sensorUpdatekBox->setObjectName(QString::fromUtf8("sensorUpdatekBox"));

        horizontalLayout_8->addWidget(sensorUpdatekBox);


        verticalLayout_4->addLayout(horizontalLayout_8);

        tabWidget->addTab(tab_5, QString());
        tab_2 = new QWidget();
        tab_2->setObjectName(QString::fromUtf8("tab_2"));
        horizontalLayout_2 = new QHBoxLayout(tab_2);
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setContentsMargins(11, 11, 11, 11);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        settingsWidget = new SettingsWidget(tab_2);
        settingsWidget->setObjectName(QString::fromUtf8("settingsWidget"));

        horizontalLayout_2->addWidget(settingsWidget);

        tabWidget->addTab(tab_2, QString());

        horizontalLayout->addWidget(tabWidget);

        MainWindow->setCentralWidget(centralWidget);
        menuBar = new QMenuBar(MainWindow);
        menuBar->setObjectName(QString::fromUtf8("menuBar"));
        menuBar->setGeometry(QRect(0, 0, 684, 25));
        MainWindow->setMenuBar(menuBar);
        statusBar = new QStatusBar(MainWindow);
        statusBar->setObjectName(QString::fromUtf8("statusBar"));
        MainWindow->setStatusBar(statusBar);

        retranslateUi(MainWindow);

        tabWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
        MainWindow->setWindowTitle(QApplication::translate("MainWindow", "MainWindow", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("MainWindow", "IP-Address", 0, QApplication::UnicodeUTF8));
        ipAddressEdit->setText(QApplication::translate("MainWindow", "127.0.0.1", 0, QApplication::UnicodeUTF8));
        label_2->setText(QApplication::translate("MainWindow", "Port", 0, QApplication::UnicodeUTF8));
        applyButton->setText(QApplication::translate("MainWindow", "Apply", 0, QApplication::UnicodeUTF8));
        sendJoystickCheckBox->setText(QApplication::translate("MainWindow", "Drive", 0, QApplication::UnicodeUTF8));
        connectJoystickButton->setText(QApplication::translate("MainWindow", "Connect Joystick", 0, QApplication::UnicodeUTF8));
        stopCameraButton->setText(QApplication::translate("MainWindow", "Stop Camera", 0, QApplication::UnicodeUTF8));
        connectCameraButton->setText(QApplication::translate("MainWindow", "Stream Camera", 0, QApplication::UnicodeUTF8));
        tabWidget->setTabText(tabWidget->indexOf(tab), QApplication::translate("MainWindow", "Connection", 0, QApplication::UnicodeUTF8));
        label_3->setText(QApplication::translate("MainWindow", "Motor Battery Voltage (V)", 0, QApplication::UnicodeUTF8));
        label_4->setText(QApplication::translate("MainWindow", "Logic Battery Voltage (V)", 0, QApplication::UnicodeUTF8));
        label_5->setText(QApplication::translate("MainWindow", "Temperature (Deg C)", 0, QApplication::UnicodeUTF8));
        label_7->setText(QApplication::translate("MainWindow", "Speed (m/s)", 0, QApplication::UnicodeUTF8));
        label_8->setText(QApplication::translate("MainWindow", "Motor Battery Charge (%)", 0, QApplication::UnicodeUTF8));
        label_9->setText(QApplication::translate("MainWindow", "Logic Battery Charge (%)", 0, QApplication::UnicodeUTF8));
        updateReadingskBox->setText(QApplication::translate("MainWindow", "Update Readings", 0, QApplication::UnicodeUTF8));
        tabWidget->setTabText(tabWidget->indexOf(tab_3), QApplication::translate("MainWindow", "Readings", 0, QApplication::UnicodeUTF8));
        label_6->setText(QApplication::translate("MainWindow", "X", 0, QApplication::UnicodeUTF8));
        label_10->setText(QApplication::translate("MainWindow", "Y", 0, QApplication::UnicodeUTF8));
        label_11->setText(QApplication::translate("MainWindow", "Alpha", 0, QApplication::UnicodeUTF8));
        setPositionButton->setText(QApplication::translate("MainWindow", "Set Position", 0, QApplication::UnicodeUTF8));
        updatePosBox->setText(QApplication::translate("MainWindow", "Update position from car", 0, QApplication::UnicodeUTF8));
        tabWidget->setTabText(tabWidget->indexOf(tab_4), QApplication::translate("MainWindow", "Map", 0, QApplication::UnicodeUTF8));
        label_12->setText(QApplication::translate("MainWindow", "Ultra 1", 0, QApplication::UnicodeUTF8));
        sensor1Bar->setFormat(QApplication::translate("MainWindow", "%v cm", 0, QApplication::UnicodeUTF8));
        label_13->setText(QApplication::translate("MainWindow", "Ultra 2", 0, QApplication::UnicodeUTF8));
        sensor2Bar->setFormat(QApplication::translate("MainWindow", "%v cm", 0, QApplication::UnicodeUTF8));
        label_14->setText(QApplication::translate("MainWindow", "Ultra 3", 0, QApplication::UnicodeUTF8));
        sensor3Bar->setFormat(QApplication::translate("MainWindow", "%v cm", 0, QApplication::UnicodeUTF8));
        label_15->setText(QApplication::translate("MainWindow", "IR 1", 0, QApplication::UnicodeUTF8));
        sensor1Bar_ir->setFormat(QApplication::translate("MainWindow", "%v", 0, QApplication::UnicodeUTF8));
        label_16->setText(QApplication::translate("MainWindow", "IR 2", 0, QApplication::UnicodeUTF8));
        sensor2Bar_ir->setFormat(QApplication::translate("MainWindow", "%v", 0, QApplication::UnicodeUTF8));
        label_17->setText(QApplication::translate("MainWindow", "IR 3", 0, QApplication::UnicodeUTF8));
        sensor3Bar_ir->setFormat(QApplication::translate("MainWindow", "%v", 0, QApplication::UnicodeUTF8));
        sensorUpdatekBox->setText(QApplication::translate("MainWindow", "Update Sensors", 0, QApplication::UnicodeUTF8));
        tabWidget->setTabText(tabWidget->indexOf(tab_5), QApplication::translate("MainWindow", "Sensors", 0, QApplication::UnicodeUTF8));
        tabWidget->setTabText(tabWidget->indexOf(tab_2), QApplication::translate("MainWindow", "Settings", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINDOW_H
