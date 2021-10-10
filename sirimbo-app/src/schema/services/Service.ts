/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */
import { request as __request } from '../core/request';

export class Service {

    /**
     * @returns any
     * @throws ApiError
     */
    public static async getService(): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/wp/v2/types`,
        });
        return result.body;
    }

    /**
     * @param id
     * @returns any
     * @throws ApiError
     */
    public static async getService1(
        id: number,
    ): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/api/schedule/${id}/toggle-visible`,
            errors: {
                404: `id not found`,
            },
        });
        return result.body;
    }

    /**
     * @returns any
     * @throws ApiError
     */
    public static async getPhotoDirectories(): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/api/photo/directory`,
        });
        return result.body;
    }

    /**
     * @param id
     * @returns any
     * @throws ApiError
     */
    public static async toggleVisibleReservation(
        id: number,
    ): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/api/reservation/${id}/toggle-visible`,
            errors: {
                404: `id not found`,
            },
        });
        return result.body;
    }

    /**
     * @param id
     * @returns any
     * @throws ApiError
     */
    public static async getAnnouncement(
        id: number,
    ): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/api/announcement/${id}`,
            errors: {
                404: `id not found`,
            },
        });
        return result.body;
    }

    /**
     * @returns any
     * @throws ApiError
     */
    public static async getService2(): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/wp/v2/taxonomies`,
        });
        return result.body;
    }

    /**
     * @param acc
     * @param am
     * @param msg
     * @param ss
     * @param vs
     * @param ks
     * @returns any
     * @throws ApiError
     */
    public static async getService3(
        acc?: string,
        am?: string,
        msg?: string,
        ss?: string,
        vs?: string,
        ks?: string,
    ): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/api/qr-payment.png`,
            query: {
                'acc': acc,
                'am': am,
                'msg': msg,
                'ss': ss,
                'vs': vs,
                'ks': ks,
            },
            errors: {
                400: `Invalid ks or vs or ss or msg or am or acc`,
            },
        });
        return result.body;
    }

    /**
     * @param page
     * @returns any
     * @throws ApiError
     */
    public static async getService4(
        page: number,
    ): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/wp/v2/pages/${page}`,
            errors: {
                404: `page not found`,
            },
        });
        return result.body;
    }

    /**
     * @returns any
     * @throws ApiError
     */
    public static async getService5(): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/wp/v2/themes`,
        });
        return result.body;
    }

    /**
     * @param id
     * @returns any
     * @throws ApiError
     */
    public static async getReservation(
        id: number,
    ): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/api/reservation/${id}`,
            errors: {
                404: `id not found`,
            },
        });
        return result.body;
    }

    /**
     * @param page
     * @returns any
     * @throws ApiError
     */
    public static async getService6(
        page: number,
    ): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/wp/v2/pages/${page}/autosaves`,
            errors: {
                404: `page not found`,
            },
        });
        return result.body;
    }

    /**
     * @returns any
     * @throws ApiError
     */
    public static async getService7(): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/api/whoami`,
        });
        return result.body;
    }

    /**
     * @returns void
     * @throws ApiError
     */
    public static async getService8(): Promise<void> {
        const result = await __request({
            method: 'GET',
            path: `/logout`,
        });
        return result.body;
    }

    /**
     * @returns string
     * @throws ApiError
     */
    public static async optionsService(): Promise<string> {
        const result = await __request({
            method: 'OPTIONS',
            path: `/wp/v2/media`,
            responseHeader: 'Allow',
        });
        return result.body;
    }

    /**
     * @param id
     * @returns any
     * @throws ApiError
     */
    public static async toggleVisiblePhotoDirectory(
        id: number,
    ): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/api/photo/directory/${id}/toggle-visible`,
            errors: {
                404: `id not found`,
            },
        });
        return result.body;
    }

    /**
     * @param id
     * @returns any
     * @throws ApiError
     */
    public static async getService9(
        id: number,
    ): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/api/event/${id}/toggle-visible`,
            errors: {
                404: `id not found`,
            },
        });
        return result.body;
    }

    /**
     * @returns any
     * @throws ApiError
     */
    public static async getService10(): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/api/export-emails`,
        });
        return result.body;
    }

    /**
     * @returns any
     * @throws ApiError
     */
    public static async getService11(): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/wp/v2/blocks`,
        });
        return result.body;
    }

    /**
     * @returns string
     * @throws ApiError
     */
    public static async optionsService1(): Promise<string> {
        const result = await __request({
            method: 'OPTIONS',
            path: `/wp/v2/blocks`,
            responseHeader: 'Allow',
        });
        return result.body;
    }

    /**
     * @returns any
     * @throws ApiError
     */
    public static async getService12(): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/wp/v2/users`,
        });
        return result.body;
    }

    /**
     * @param type
     * @returns any
     * @throws ApiError
     */
    public static async getService13(
        type: string,
    ): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/wp/v2/types/${type}`,
            errors: {
                404: `type not found`,
            },
        });
        return result.body;
    }

    /**
     * @returns any
     * @throws ApiError
     */
    public static async getService14(): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/wp/v2/users/me`,
        });
        return result.body;
    }

    /**
     * @param offset
     * @param limit
     * @returns any
     * @throws ApiError
     */
    public static async getAnnouncements(
        offset?: number,
        limit?: number,
    ): Promise<any> {
        const result = await __request({
            method: 'GET',
            path: `/api/announcement`,
            query: {
                'offset': offset,
                'limit': limit,
            },
            errors: {
                400: `Invalid limit or offset`,
            },
        });
        return result.body;
    }

}