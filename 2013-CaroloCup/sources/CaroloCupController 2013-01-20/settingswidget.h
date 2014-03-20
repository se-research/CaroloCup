#ifndef SETTINGSWIDGET_H
#define SETTINGSWIDGET_H

#include <QWidget>
#include <QFile>

namespace Ui {
    class SettingsWidget;
}

enum JoystickType {JS_WHEEL, JS_XBOX_360, JS_XBOX_360_2, JS_RC};

struct settings_t {
    QString serialPort;
    QString joystickPort;
    JoystickType joystickType;
    int baudrate;
    double topVal;
    int maxVal;
    int hysteres;
    int stepSize;
    int steeringOffset;
};

class SettingsWidget : public QWidget
{
    Q_OBJECT

public:
    SettingsWidget(QWidget *parent = 0);
    ~SettingsWidget();
    void setSettings(settings_t *settings);
    void setDefaultSettings();
    void saveSettingsToFile(QFile &savefile);
    void loadSettingsFromFile(QFile &loadfile);

Q_SIGNALS:
    void settingsUpdated();

private Q_SLOTS:
    void on_pushButtonSave_clicked();

private:
    void updateFromSettings();

    Ui::SettingsWidget *ui;
    settings_t *mSettings;
    QFile mSaveFile;
};

#endif // SETTINGSWIDGET_H
