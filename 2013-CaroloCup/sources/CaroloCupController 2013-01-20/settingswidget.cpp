#include <QFileDialog>
#include <QDebug>
#include "settingswidget.h"
#include "ui_settingswidget.h"
#include <qxmlstream.h>
#include <qfile.h>
#include <QString>
#include <QMessageBox>




SettingsWidget::SettingsWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::SettingsWidget),
    mSaveFile("SavedSettings.xml")
{
    ui->setupUi(this);
}

SettingsWidget::~SettingsWidget()
{
    delete ui;
}

void SettingsWidget::setSettings(settings_t *settings)
{
    mSettings = settings;
}

void SettingsWidget::loadSettingsFromFile(QFile &loadfile){

    if (!loadfile.open(QFile::ReadOnly | QFile::Text)) {
        qWarning() << "Error loading settings";
        return;
    }

    QXmlStreamReader xmlreader(&loadfile);
    xmlreader.readNext();

    while (!xmlreader.atEnd()) {
        if (xmlreader.isStartElement()) {
            if (xmlreader.name() == "Settings") {
                xmlreader.readNext();
                while (!xmlreader.atEnd()) {
                    if (xmlreader.isEndElement()) {
                        xmlreader.readNext();
                        break;
                    }
                    bool result;
                    if(xmlreader.name() == "joystickPort"){
                        mSettings->joystickPort=xmlreader.readElementText();
                    }
                    else if(xmlreader.name() == "serialPort"){
                        mSettings->serialPort=xmlreader.readElementText();
                    }
                    else if (xmlreader.name() == "baudrate") {
                        mSettings->baudrate=xmlreader.readElementText().toInt(&result,10);
                    }
                    else if (xmlreader.name() == "topVal") {
                        mSettings->topVal=xmlreader.readElementText().toDouble(&result);
                    }
                    else if (xmlreader.name() == "maxVal") {
                        mSettings->maxVal=xmlreader.readElementText().toInt(&result,10);
                    }
                    else if (xmlreader.name() == "hysteres") {
                        mSettings->hysteres=xmlreader.readElementText().toInt(&result,10);
                    }
                    else if (xmlreader.name() == "stepSize") {
                        mSettings->stepSize=xmlreader.readElementText().toInt(&result,10);
                    }
                    else if (xmlreader.name() == "steeringOffset") {
                        mSettings->steeringOffset=xmlreader.readElementText().toInt(&result,10);
                    }
                    xmlreader.readNext();
                }
            } else {
                qWarning() << "Not a settings file";
                break;
            }
        } else {
            xmlreader.readNext();
        }
    }
   loadfile.close();
   updateFromSettings();
}

void SettingsWidget::saveSettingsToFile(QFile &savefile){
    if (!savefile.open(QFile::WriteOnly | QFile::Text)) {
        qWarning() << "Error saving settings to file";
        return;
    }

    on_pushButtonSave_clicked();
    QXmlStreamWriter xmlwriter(&savefile);
    xmlwriter.setAutoFormatting(true);
    xmlwriter.writeStartDocument();
    xmlwriter.writeStartElement("Settings");
    xmlwriter.writeTextElement("joystickPort",mSettings->joystickPort);//Joystick Type is not Saved
    xmlwriter.writeTextElement("serialPort", mSettings->serialPort);
    xmlwriter.writeTextElement("baudrate", QString::number(mSettings->baudrate, 10));
    xmlwriter.writeTextElement("topVal", QString::number(mSettings->topVal));
    xmlwriter.writeTextElement("maxVal", QString::number(mSettings->maxVal,10));
    xmlwriter.writeTextElement("hysteres", QString::number(mSettings->hysteres,10));
    xmlwriter.writeTextElement("stepSize", QString::number(mSettings->stepSize,10));
    xmlwriter.writeTextElement("steeringOffset", QString::number(mSettings->steeringOffset,10));

    xmlwriter.writeEndElement();
    xmlwriter.writeEndDocument();
    savefile.close();
}

void SettingsWidget::setDefaultSettings()
{
    if (mSettings != 0)
    {
        mSettings->joystickPort = "/dev/input/js0";
        mSettings->serialPort = "/dev/rfcomm0";
        mSettings->joystickType = JS_XBOX_360_2;
        mSettings->baudrate = 115200;
        mSettings->topVal = 2.0;
        mSettings->maxVal = 40000;
        mSettings->hysteres = 15;
        mSettings->stepSize = 2500;
        mSettings->steeringOffset = 0;
        updateFromSettings();
    }
}

void SettingsWidget::updateFromSettings()
{
    if (mSettings != 0)
    {
        ui->lineEditJoystick->setText(mSettings->joystickPort);
        ui->lineEditSerialPort->setText(mSettings->serialPort);
        ui->spinBoxHysteres->setValue(mSettings->hysteres);
        ui->spinBoxMaxVal->setValue(mSettings->maxVal);
        ui->spinBoxStep->setValue(mSettings->stepSize);
        ui->spinBoxSteeringOffset->setValue(mSettings->steeringOffset);
        ui->maxSpeedSpinBox->setValue(mSettings->topVal);
    }
}

void SettingsWidget::on_pushButtonSave_clicked()
{
    if (mSettings != 0)
    {
        mSettings->joystickPort = ui->lineEditJoystick->text();
        mSettings->serialPort = ui->lineEditSerialPort->text();
        mSettings->hysteres = ui->spinBoxHysteres->value();
        mSettings->maxVal = ui->spinBoxMaxVal->value();
        mSettings->topVal = ui->maxSpeedSpinBox->value();
        mSettings->stepSize = ui->spinBoxStep->value();
        mSettings->steeringOffset = ui->spinBoxSteeringOffset->value();
        Q_EMIT settingsUpdated();
    }
}
