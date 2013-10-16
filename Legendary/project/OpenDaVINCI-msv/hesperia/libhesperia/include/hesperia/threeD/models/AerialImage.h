/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_THREED_MODELS_AERIALIMAGE_H_
#define HESPERIA_CORE_THREED_MODELS_AERIALIMAGE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/wrapper/Image.h"

#include "core/data/environment/Point3.h"
#include "hesperia/threeD/Node.h"
#include "hesperia/threeD/NodeDescriptor.h"
#include "hesperia/threeD/TransformGroup.h"

namespace hesperia {
    namespace threeD {
        namespace models {

            using namespace std;

            /**
             * This class represents the actual renderer.
             */
            class OPENDAVINCI_API AerialImageRenderer : public Node {
                public:
                    /**
                     * Constructor.
                     *
                    * @param nodeDesciptor Description for this node.
                     * @param textureHandle Handle for the image's data inside an OpenGL texture.
                     */
                    AerialImageRenderer(const NodeDescriptor &nodeDescriptor, const uint32_t &textureHandle);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    AerialImageRenderer(const AerialImageRenderer &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    AerialImageRenderer& operator=(const AerialImageRenderer &obj);

                    virtual ~AerialImageRenderer();

                    virtual void render(RenderingConfiguration &renderingConfiguration);

                private:
                    uint32_t m_textureHandle;
            };

            /**
             * This class an aerial image.
             */
            class OPENDAVINCI_API AerialImage : public Node {
                public:
                    /**
                     * Constructor.
                     *
                    * @param nodeDesciptor Description for this node.
                     * @param image Image to be used as aerial image.
                     * @param originPixelXY Origin pixel inside the image.
                     * @param scalingPixelXY Scaling for the pixels.
                     * @param rotationZ Rotation of the image around the Z-axis.
                     */
                    AerialImage(const NodeDescriptor &nodeDescriptor,
                                const core::wrapper::Image *image,
                                const core::data::environment::Point3 &originPixelXY,
                                const core::data::environment::Point3 &scalingPixelXY,
                                const float &rotationZ);

                    /**
                     * Copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    AerialImage(const AerialImage &obj);

                    /**
                     * Assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    AerialImage& operator=(const AerialImage &obj);

                    virtual ~AerialImage();

                    virtual void render(RenderingConfiguration &renderingConfiguration);

                private:
                    const core::wrapper::Image *m_image;
                    core::data::environment::Point3 m_originPixelXY;
                    core::data::environment::Point3 m_scalingPixelXY;
                    float m_rotationZ;

                    uint32_t m_textureHandle;
                    TransformGroup *m_aerialImageNode;
                    AerialImageRenderer *m_aerialImageRenderer;
                    TransformGroup *m_translateToTheCenterOfTheImage;
                    TransformGroup *m_flipImageAroundXAxis;
                    TransformGroup *m_translateBack;

                    /**
                     * This method initializes the aerial image.
                     */
                    void init();
            };

        }
    }
} // hesperia::threeD::models

#endif /*HESPERIA_CORE_THREED_MODELS_AERIALIMAGE_H_*/
