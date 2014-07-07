/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "hesperia/threeD/Material.h"

namespace hesperia {
    namespace threeD {

        using namespace core::data::environment;

        Material::Material() :
                m_name("Undefined"),
                m_textureName(),
                m_textureHandle(-1),
                m_shininess(0),
                m_ambient(),
                m_diffuse(1, 1, 1),
                m_specular() {}

        Material::Material(const string &name) :
                m_name(name),
                m_textureName(),
                m_textureHandle(-1),
                m_shininess(0),
                m_ambient(),
                m_diffuse(1, 1, 1),
                m_specular() {}

        Material::Material(const Material &obj) :
                m_name(obj.m_name),
                m_textureName(obj.m_textureName),
                m_textureHandle(obj.m_textureHandle),
                m_shininess(obj.m_shininess),
                m_ambient(obj.m_ambient),
                m_diffuse(obj.m_diffuse),
                m_specular(obj.m_specular) {}

        Material::~Material() {}

        Material& Material::operator=(const Material &obj) {
            m_name = obj.m_name;
            m_textureName = obj.m_textureName;
            m_textureHandle = obj.m_textureHandle;
            m_shininess = obj.m_shininess;
            m_ambient = obj.m_ambient;
            m_diffuse = obj.m_diffuse;
            m_specular = obj.m_specular;

            return (*this);
        }

        const string Material::getName() const {
            return m_name;
        }

        void Material::setTextureName(const string &textureName) {
            m_textureName = textureName;
        }

        const string Material::getTextureName() const {
            return m_textureName;
        }

        void Material::setShininess(const double &s) {
            m_shininess = s;
        }

        double Material::getShininess() const {
            return m_shininess;
        }

        void Material::setAmbient(const Point3 &a) {
            m_ambient = a;
        }

        const Point3 Material::getAmbient() const {
            return m_ambient;
        }

        void Material::setDiffuse(const Point3 &d) {
            m_diffuse = d;
        }

        const Point3 Material::getDiffuse() const {
            return m_diffuse;
        }

        void Material::setSpecular(const Point3 &s) {
            m_specular = s;
        }

        const Point3 Material::getSpecular() const {
            return m_specular;
        }

        void Material::setTextureHandle(const int32_t &textureHandle) {
            m_textureHandle = textureHandle;
        }

        int32_t Material::getTextureHandle() const {
            return m_textureHandle;
        }

    }
} // hesperia::threeD
