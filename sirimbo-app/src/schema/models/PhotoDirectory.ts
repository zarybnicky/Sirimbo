/* tslint:disable */
/* eslint-disable */
/**
 * 
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 1.0.0
 * 
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 */

import { exists, mapValues } from '../runtime';
/**
 * 
 * @export
 * @interface PhotoDirectory
 */
export interface PhotoDirectory {
    /**
     * 
     * @type {string}
     * @memberof PhotoDirectory
     */
    photoDirectoryHidden: string;
    /**
     * 
     * @type {string}
     * @memberof PhotoDirectory
     */
    photoDirectoryName: string;
    /**
     * 
     * @type {number}
     * @memberof PhotoDirectory
     */
    photoDirectoryLevel: number;
    /**
     * 
     * @type {string}
     * @memberof PhotoDirectory
     */
    photoDirectoryPath: string;
    /**
     * 
     * @type {number}
     * @memberof PhotoDirectory
     */
    photoDirectoryParent: number;
}

export function PhotoDirectoryFromJSON(json: any): PhotoDirectory {
    return PhotoDirectoryFromJSONTyped(json, false);
}

export function PhotoDirectoryFromJSONTyped(json: any, ignoreDiscriminator: boolean): PhotoDirectory {
    if ((json === undefined) || (json === null)) {
        return json;
    }
    return {
        
        'photoDirectoryHidden': json['photoDirectoryHidden'],
        'photoDirectoryName': json['photoDirectoryName'],
        'photoDirectoryLevel': json['photoDirectoryLevel'],
        'photoDirectoryPath': json['photoDirectoryPath'],
        'photoDirectoryParent': json['photoDirectoryParent'],
    };
}

export function PhotoDirectoryToJSON(value?: PhotoDirectory | null): any {
    if (value === undefined) {
        return undefined;
    }
    if (value === null) {
        return null;
    }
    return {
        
        'photoDirectoryHidden': value.photoDirectoryHidden,
        'photoDirectoryName': value.photoDirectoryName,
        'photoDirectoryLevel': value.photoDirectoryLevel,
        'photoDirectoryPath': value.photoDirectoryPath,
        'photoDirectoryParent': value.photoDirectoryParent,
    };
}

