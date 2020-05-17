import * as _ from 'lodash';
import * as React from 'react';
import * as ReactDOM from 'react-dom';
import * as moment from 'moment';

window.lodash = _.noConflict();

import '@wordpress/hooks';

import '@wordpress/i18n';
import '@wordpress/url';
import '@wordpress/api-fetch';
import '@wordpress/blob';
import '@wordpress/autop';
import '@wordpress/block-serialization-default-parser';

import '@wordpress/escape-html';
import '@wordpress/element';
import '@wordpress/is-shallow-equal';
import '@wordpress/compose';
import '@wordpress/priority-queue';
import '@wordpress/redux-routine';
import '@wordpress/data';
// import './g-data';
import '@wordpress/dom';
import '@wordpress/html-entities';
import '@wordpress/shortcode';
import '@wordpress/blocks';

import '@wordpress/keycodes';
import '@wordpress/rich-text';
import '@wordpress/dom-ready';
import '@wordpress/a11y';
import '@wordpress/deprecated';
import '@wordpress/components';
import '@wordpress/core-data';
import '@wordpress/token-list';
import '@wordpress/viewport';
import '@wordpress/wordcount';
import '@wordpress/block-editor';
import '@wordpress/block-directory';

import '@wordpress/date';
import '@wordpress/notices';
import '@wordpress/nux';
import '@wordpress/data-controls';
import '@wordpress/media-utils';
import '@wordpress/server-side-render';
import '@wordpress/editor';
import '@wordpress/block-library';
import '@wordpress/plugins';
import '@wordpress/edit-post';
import '@wordpress/format-library';

import '@wordpress/annotations';
import '@wordpress/block-serialization-spec-parser';
import '@wordpress/edit-widgets';
import '@wordpress/list-reusable-blocks';

import '@wordpress/components/src/style.scss';
import '@wordpress/block-editor/src/style.scss';
import '@wordpress/nux/src/style.scss';
import '@wordpress/editor/src/style.scss';
import '@wordpress/editor/src/editor-styles.scss';
import '@wordpress/block-library/src/style.scss';
import '@wordpress/block-library/src/editor.scss';
import '@wordpress/block-library/src/theme.scss';
import '@wordpress/edit-post/src/style.scss';
import '@wordpress/format-library/src/style.scss';
import '@wordpress/list-reusable-blocks/src/style.scss';
import '@wordpress/block-directory/src/style.scss';

window.userSettings = {
    secure: '',
    time: 1234567,
    uid: 1,
};

// API settings
window.wpApiSettings = {
    root: window.location.origin + '/',
    nonce: '123456789',
    versionString: 'wp/v2/',
};

// postboxes
/* window.postboxes = (window as any).postboxes || {
 *     add_postbox_toggles: (page, args) => {
 *         console.log('page', page);
 *         console.log('args', args);
 *     },
 * }; */

// import './api-fetch';
// import './media-upload';

import { dispatch } from '@wordpress/data';
import domReady from '@wordpress/dom-ready';
import { initializeEditor } from '@wordpress/edit-post';

class Editor extends React.Component {
    componentDidMount() {
        // Disable publish sidebar
        dispatch('core/editor').disablePublishSidebar();

        // Disable tips
        dispatch('core/nux').disableTips();

        // Initialize the editor
        window._wpLoadBlockEditor = new Promise(resolve => {
            domReady(() => {
                resolve(initializeEditor('editor', 'page', 1, {
                    alignWide: true,
                    availableTemplates: [],
                    allowedBlockTypes: true,
                    disableCustomColors: false,
                    disableCustomFontSizes: false,
                    disablePostFormats: false,
                    titlePlaceholder: 'Add title',
                    bodyPlaceholder: 'Insert your custom block',
                    isRTL: false,
                    autosaveInterval: 3,
                    style: [],
                    imageSizes: [],
                    richEditingEnabled: true,
                    postLock: {
                        isLocked: false,
                    },
                    postLockUtils: {
                        nonce: '123456789',
                    },
                    enableCustomFields: true,
                    mediaLibrary: true,
                }, {}));
            });
        });
    }

    resetLocalStorage = ev => {
        ev.preventDefault();
        localStorage.removeItem('g-editor-page');
        sessionStorage.removeItem('wp-autosave-block-editor-post-1');
        window.location.reload();
    };

    render() {
        return (
            <React.Fragment>
                <div className="editor-nav" >
                    <button type="button" className="components-button is-tertiary"
                        onClick={this.resetLocalStorage} > Clear page and reload </button>
                </div>
                <div id="editor" className="gutenberg__editor" > </div>
            </React.Fragment>
        );
    }
}

ReactDOM.render(<div><Editor></Editor></div>, document.getElementById('root'));
