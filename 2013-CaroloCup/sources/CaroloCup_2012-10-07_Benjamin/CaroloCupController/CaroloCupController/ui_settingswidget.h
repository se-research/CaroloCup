/********************************************************************************
** Form generated from reading UI file 'settingswidget.ui'
**
** Created: Thu Nov 29 10:08:36 2012
**      by: Qt User Interface Compiler version 4.6.2
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_SETTINGSWIDGET_H
#define UI_SETTINGSWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDoubleSpinBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QSpinBox>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_SettingsWidget
{
public:
    QVBoxLayout *verticalLayout_7;
    QTabWidget *tabWidget;
    QWidget *tab;
    QVBoxLayout *verticalLayout_5;
    QHBoxLayout *horizontalLayout_4;
    QVBoxLayout *verticalLayout;
    QLabel *label;
    QLabel *label_2;
    QVBoxLayout *verticalLayout_2;
    QLineEdit *lineEditJoystick;
    QLineEdit *lineEditSerialPort;
    QSpacerItem *verticalSpacer_2;
    QWidget *tab_2;
    QVBoxLayout *verticalLayout_6;
    QHBoxLayout *horizontalLayout;
    QVBoxLayout *verticalLayout_3;
    QLabel *label_3;
    QLabel *label_4;
    QLabel *label_5;
    QLabel *label_7;
    QLabel *label_8;
    QVBoxLayout *verticalLayout_4;
    QSpinBox *spinBoxStep;
    QSpinBox *spinBoxMaxVal;
    QSpinBox *spinBoxHysteres;
    QSpinBox *spinBoxSteeringOffset;
    QDoubleSpinBox *maxSpeedSpinBox;
    QSpacerItem *verticalSpacer;
    QHBoxLayout *horizontalLayout_3;
    QSpacerItem *horizontalSpacer;
    QPushButton *pushButtonSave;

    void setupUi(QWidget *SettingsWidget)
    {
        if (SettingsWidget->objectName().isEmpty())
            SettingsWidget->setObjectName(QString::fromUtf8("SettingsWidget"));
        SettingsWidget->resize(609, 722);
        verticalLayout_7 = new QVBoxLayout(SettingsWidget);
        verticalLayout_7->setContentsMargins(0, 0, 0, 0);
        verticalLayout_7->setObjectName(QString::fromUtf8("verticalLayout_7"));
        tabWidget = new QTabWidget(SettingsWidget);
        tabWidget->setObjectName(QString::fromUtf8("tabWidget"));
        tab = new QWidget();
        tab->setObjectName(QString::fromUtf8("tab"));
        verticalLayout_5 = new QVBoxLayout(tab);
        verticalLayout_5->setObjectName(QString::fromUtf8("verticalLayout_5"));
        horizontalLayout_4 = new QHBoxLayout();
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        verticalLayout = new QVBoxLayout();
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        label = new QLabel(tab);
        label->setObjectName(QString::fromUtf8("label"));

        verticalLayout->addWidget(label);

        label_2 = new QLabel(tab);
        label_2->setObjectName(QString::fromUtf8("label_2"));

        verticalLayout->addWidget(label_2);


        horizontalLayout_4->addLayout(verticalLayout);

        verticalLayout_2 = new QVBoxLayout();
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        lineEditJoystick = new QLineEdit(tab);
        lineEditJoystick->setObjectName(QString::fromUtf8("lineEditJoystick"));

        verticalLayout_2->addWidget(lineEditJoystick);

        lineEditSerialPort = new QLineEdit(tab);
        lineEditSerialPort->setObjectName(QString::fromUtf8("lineEditSerialPort"));

        verticalLayout_2->addWidget(lineEditSerialPort);


        horizontalLayout_4->addLayout(verticalLayout_2);


        verticalLayout_5->addLayout(horizontalLayout_4);

        verticalSpacer_2 = new QSpacerItem(20, 212, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_5->addItem(verticalSpacer_2);

        tabWidget->addTab(tab, QString());
        tab_2 = new QWidget();
        tab_2->setObjectName(QString::fromUtf8("tab_2"));
        verticalLayout_6 = new QVBoxLayout(tab_2);
        verticalLayout_6->setObjectName(QString::fromUtf8("verticalLayout_6"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        verticalLayout_3 = new QVBoxLayout();
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        label_3 = new QLabel(tab_2);
        label_3->setObjectName(QString::fromUtf8("label_3"));

        verticalLayout_3->addWidget(label_3);

        label_4 = new QLabel(tab_2);
        label_4->setObjectName(QString::fromUtf8("label_4"));

        verticalLayout_3->addWidget(label_4);

        label_5 = new QLabel(tab_2);
        label_5->setObjectName(QString::fromUtf8("label_5"));

        verticalLayout_3->addWidget(label_5);

        label_7 = new QLabel(tab_2);
        label_7->setObjectName(QString::fromUtf8("label_7"));

        verticalLayout_3->addWidget(label_7);

        label_8 = new QLabel(tab_2);
        label_8->setObjectName(QString::fromUtf8("label_8"));

        verticalLayout_3->addWidget(label_8);


        horizontalLayout->addLayout(verticalLayout_3);

        verticalLayout_4 = new QVBoxLayout();
        verticalLayout_4->setObjectName(QString::fromUtf8("verticalLayout_4"));
        spinBoxStep = new QSpinBox(tab_2);
        spinBoxStep->setObjectName(QString::fromUtf8("spinBoxStep"));
        spinBoxStep->setMaximum(10000);
        spinBoxStep->setSingleStep(500);

        verticalLayout_4->addWidget(spinBoxStep);

        spinBoxMaxVal = new QSpinBox(tab_2);
        spinBoxMaxVal->setObjectName(QString::fromUtf8("spinBoxMaxVal"));
        spinBoxMaxVal->setMaximum(100000);
        spinBoxMaxVal->setSingleStep(5000);

        verticalLayout_4->addWidget(spinBoxMaxVal);

        spinBoxHysteres = new QSpinBox(tab_2);
        spinBoxHysteres->setObjectName(QString::fromUtf8("spinBoxHysteres"));
        spinBoxHysteres->setMaximum(127);

        verticalLayout_4->addWidget(spinBoxHysteres);

        spinBoxSteeringOffset = new QSpinBox(tab_2);
        spinBoxSteeringOffset->setObjectName(QString::fromUtf8("spinBoxSteeringOffset"));
        spinBoxSteeringOffset->setMinimum(-128);
        spinBoxSteeringOffset->setMaximum(127);

        verticalLayout_4->addWidget(spinBoxSteeringOffset);

        maxSpeedSpinBox = new QDoubleSpinBox(tab_2);
        maxSpeedSpinBox->setObjectName(QString::fromUtf8("maxSpeedSpinBox"));

        verticalLayout_4->addWidget(maxSpeedSpinBox);


        horizontalLayout->addLayout(verticalLayout_4);


        verticalLayout_6->addLayout(horizontalLayout);

        verticalSpacer = new QSpacerItem(20, 214, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_6->addItem(verticalSpacer);

        tabWidget->addTab(tab_2, QString());

        verticalLayout_7->addWidget(tabWidget);

        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer);

        pushButtonSave = new QPushButton(SettingsWidget);
        pushButtonSave->setObjectName(QString::fromUtf8("pushButtonSave"));

        horizontalLayout_3->addWidget(pushButtonSave);


        verticalLayout_7->addLayout(horizontalLayout_3);


        retranslateUi(SettingsWidget);

        tabWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(SettingsWidget);
    } // setupUi

    void retranslateUi(QWidget *SettingsWidget)
    {
        SettingsWidget->setWindowTitle(QApplication::translate("SettingsWidget", "Form", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("SettingsWidget", "Joystick", 0, QApplication::UnicodeUTF8));
        label_2->setText(QApplication::translate("SettingsWidget", "Serial Port", 0, QApplication::UnicodeUTF8));
        lineEditJoystick->setText(QString());
        lineEditSerialPort->setText(QString());
        tabWidget->setTabText(tabWidget->indexOf(tab), QApplication::translate("SettingsWidget", "Paths", 0, QApplication::UnicodeUTF8));
        label_3->setText(QApplication::translate("SettingsWidget", "PWR Stepsize", 0, QApplication::UnicodeUTF8));
        label_4->setText(QApplication::translate("SettingsWidget", "PWR Maxval", 0, QApplication::UnicodeUTF8));
        label_5->setText(QApplication::translate("SettingsWidget", "PWR-axis Hysteres", 0, QApplication::UnicodeUTF8));
        label_7->setText(QApplication::translate("SettingsWidget", "Steering offset", 0, QApplication::UnicodeUTF8));
        label_8->setText(QApplication::translate("SettingsWidget", "Joystick max speed (m/s)", 0, QApplication::UnicodeUTF8));
        tabWidget->setTabText(tabWidget->indexOf(tab_2), QApplication::translate("SettingsWidget", "Numerical values", 0, QApplication::UnicodeUTF8));
        pushButtonSave->setText(QApplication::translate("SettingsWidget", "Save changes", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class SettingsWidget: public Ui_SettingsWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_SETTINGSWIDGET_H
