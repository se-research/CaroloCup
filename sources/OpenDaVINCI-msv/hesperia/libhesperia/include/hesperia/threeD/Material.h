/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_MATERIAL_H_
#define HESPERIA_CORE_THREED_MATERIAL_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/environment/Point3.h"

namespace hesperia {
    namespace threeD {

        using namespace std;

        /**
         * This class represents a material description.
         */
        class Material {
            public:
                Material();

                /**
                 * Constructor.
                 *
                 * @param name Name of this material description.
                 */
                Material(const string &name);

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                Material(const Material &obj);

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                Material& operator=(const Material &obj);

                virtual ~Material();

                /**
                 * This method returns the name of this material description.
                 *
                 * @return Material's name.
                 */
                const string getName() const;

                /**
                 * This method sets the name of the texture for this material description.
                 *
                 * @param textureName Name of the texture for this material.
                 */
                void setTextureName(const string &textureName);

                /**
                 * This method returns the teture's name of this material.
                 *
                 * @return Texture's name.
                 */
                const string getTextureName() const;

                /**
                 * This method sets the shininess factor.
                 *
                 * @param s Shininess.
                 */
                void setShininess(const double &s);

                /**
                 * This method returns the shininess factor.
                 *
                 * @return Shininess.
                 */
                double getShininess() const;

                /**
                 * This method sets the value for ambient.
                 *
                 * @param a Value for ambient.
                 */
                void setAmbient(const core::data::environment::Point3 &a);

                /**
                 * This method returns the value for ambient.
                 *
                 * @return Value for ambient.
                 */
                const core::data::environment::Point3 getAmbient() const;

                /**
                 * This method sets the value for diffuse.
                 *
                 * @param d Value for diffuse.
                 */
                void setDiffuse(const core::data::environment::Point3 &d);

                /**
                 * This method returns the value for diffuse.
                 *
                 * @return Value for diffuse.
                 */
                const core::data::environment::Point3 getDiffuse() const;

                /**
                 * This method sets the value for specular.
                 *
                 * @param s Value for specular.
                 */
                void setSpecular(const core::data::environment::Point3 &s);

                /**
                 * This method returns the value for specular.
                 *
                 * @return Value for specular.
                 */
                const core::data::environment::Point3 getSpecular() const;

                /**
                 * This method sets the texture handle.
                 *
                 * @param textureHandle Texture handle;
                 */
                void setTextureHandle(const int32_t &textureHandle);

                /**
                 * This method returns the texture handle to be used.
                 * If the handle is -1, no texture is found.
                 *
                 * @return Texture handle.
                 */
                int32_t getTextureHandle() const;

            private:
                string m_name;
                string m_textureName;
                int32_t m_textureHandle;
                double m_shininess;

                core::data::environment::Point3 m_ambient;
                core::data::environment::Point3 m_diffuse;
                core::data::environment::Point3 m_specular;
        };

    }
} // hesperia::threeD

#endif /*HESPERIA_CORE_THREED_MATERIAL_H_*/
