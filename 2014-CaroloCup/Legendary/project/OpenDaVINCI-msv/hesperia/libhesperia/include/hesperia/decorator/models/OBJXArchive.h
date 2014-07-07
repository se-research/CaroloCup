/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_DECORATOR_MODELS_OBJXARCHIVE_H_
#define HESPERIA_CORE_DECORATOR_MODELS_OBJXARCHIVE_H_

#include <map>
#include <string>
#include <sstream>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/wrapper/Image.h"
#include "core/wrapper/StringComparator.h"

#include "hesperia/decorator/models/Material.h"
#include "hesperia/decorator/models/TriangleSet.h"

namespace hesperia {
    namespace decorator {
        namespace models {

            using namespace std;

            // Forward declaration to prevent circular dependencies.
            class OBJXArchiveFactory;

            /**
             * This class represents the contents of an SCNX archive.
             */
            class OPENDAVINCI_API OBJXArchive {
                private:
                    friend class OBJXArchiveFactory;

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     *
                     * @param obj Reference to an object of this class.
                     */
                    OBJXArchive(const OBJXArchive&);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     *
                     * @param obj Reference to an object of this class.
                     * @return Reference to this instance.
                     */
                    OBJXArchive& operator=(const OBJXArchive&);

                private:
                    /**
                     * Constructor.
                     */
                    OBJXArchive();

                public:
                    virtual ~OBJXArchive();

                    /**
                     * This method adds a new image for texturing the
                     * model by reading the MTL file.
                     *
                     * @param name Name of the image to be added.
                     * @param image Image to be added.
                     */
                    void addImage(const string &name, core::wrapper::Image *image);

                    /**
                     * This method returns a list of registered images.
                     *
                     * @return List of registered image names.
                     */
                    vector<string> getListOfImages() const;

                    /**
                     * This method sets the content of the obj-file for further
                     * processing.
                     *
                     * @param objContents Contents of the obj-file.
                     */
                    void setContentsOfObjFile(const string &objContents);

                    /**
                     * This method sets the content of the mtl-file for further
                     * processing.
                     *
                     * @param mtlContents Contents of the mtl-file.
                     */
                    void setContentsOfMtlFile(const string &mtlContents);

                    /**
                     * This method returns a reference to the contents of
                     * the obj-file.
                     *
                     * @return Reference to the contents of the obj-file.
                     */
                    const stringstream& getContentsOfObjFile() const;

                    /**
                     * This method returns a reference to the contents of
                     * the mtl-file.
                     *
                     * @return Reference to the contents of the mtl-file.
                     */
                    const stringstream& getContentsOfMtlFile() const;

                    /**
                     * This method returns the list of renderable TriangleSets.
                     *
                     * @return List of renderable triangle sets.
                     */
                    const vector<TriangleSet> getListOfTriangleSets();

                private:
                    map<string, core::wrapper::Image*, core::wrapper::StringComparator> m_mapOfImages;
                    map<string, Material, core::wrapper::StringComparator> m_mapOfMaterials;
                    stringstream m_objFile;
                    stringstream m_mtlFile;

                    /**
                     * This method creates the material's map.
                     */
                    void createMapOfMaterials();
            };

        }
    }
} // hesperia::decorator::models

#endif /*HESPERIA_CORE_DECORATOR_MODELS_OBJXARCHIVE_H_*/
