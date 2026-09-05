import { CKEditor } from '@ckeditor/ckeditor5-react';
import { ImageLibraryDialog } from '@/ui/forms/FilePicker';
import {
  type PluginFunctionConstructor,
  Autoformat,
  ButtonView,
  SourceEditing,
  ClassicEditor,
  Essentials,
  Bold,
  Italic,
  Heading,
  Image,
  ImageCaption,
  ImageStyle,
  ImageToolbar,
  Indent,
  Link,
  List,
  Paragraph,
  PasteFromOffice,
  Table,
  TableToolbar,
  TextTransformation,
  AutoImage,
  FileRepository,
  type FileLoader,
  ImageInsert,
  ImageInsertViaUrl,
  ImageInsertUI,
  MenuBarMenuListItemButtonView,
  GeneralHtmlSupport,
} from 'ckeditor5';
import React from 'react';

import 'ckeditor5/ckeditor5.css';

export type EditorProps = {
  name: string;
  onChange?: (state: string) => void;
  onBlur?: () => void;
  initialState?: string;
  enableImageUpload?: boolean;
};

export default function Editor(props: EditorProps) {
  const { name, onChange, onBlur, initialState, enableImageUpload = false } = props;
  const realInitial = React.useMemo(() => {
    return decodeHTML(initialState);
  }, [initialState]);

  const [editor, setEditor] = React.useState<ClassicEditor | null>(null);
  const [value, setValue] = React.useState(realInitial);
  const [libraryOpen, setLibraryOpen] = React.useState(false);
  const fileLibraryPlugin = React.useMemo(
    () => createFileLibraryIntegrationPlugin(() => setLibraryOpen(true)),
    [],
  );

  React.useEffect(() => {
    if (editor) {
      editor.setData(realInitial);
      setValue(realInitial);
    }
  }, [editor, realInitial]);

  const cb = React.useCallback(
    (_: unknown, editor: ClassicEditor) => {
      onChange?.(editor.getData());
      setValue(editor.getData());
    },
    [onChange],
  );
  const insertLibraryImage = React.useCallback(
    (url: string) => {
      if (!editor) return;

      const replaceImageSource = editor.commands.get('replaceImageSource');
      if (replaceImageSource?.isEnabled) {
        editor.execute('replaceImageSource', { source: url });
      } else {
        editor.execute('insertImage', { source: url });
      }
      editor.editing.view.focus();
    },
    [editor],
  );

  const _ckContent = <div className="ck-content" />; // for tailwind's JIT
  return (
    <>
      <input type="hidden" name={name} value={value} />
      <CKEditor
        editor={ClassicEditor}
        config={{
          licenseKey: 'GPL',
          plugins: [
            Essentials,
            AutoImage,
            Autoformat,
            Bold,
            Italic,
            GeneralHtmlSupport,
            Heading,
            Image,
            ImageCaption,
            enableImageUpload ? ImageInsert : ImageInsertViaUrl,
            ImageStyle,
            ImageToolbar,
            Indent,
            Link,
            List,
            Paragraph,
            PasteFromOffice,
            Table,
            TableToolbar,
            TextTransformation,
            SourceEditing,
            EditorClassPlugin as PluginFunctionConstructor,
            ...(enableImageUpload
              ? [
                  FileUploadAdapterPlugin as PluginFunctionConstructor,
                  fileLibraryPlugin as PluginFunctionConstructor,
                ]
              : []),
          ],
          toolbar: [
            'undo',
            'redo',
            '|',
            'heading',
            '|',
            'bold',
            'italic',
            '|',
            'link',
            enableImageUpload ? 'insertImage' : 'insertImageViaUrl',
            'insertTable',
            '|',
            'bulletedList',
            'numberedList',
            'outdent',
            'indent',
            'sourceEditing',
          ],
          htmlSupport: {
            allow: [
              {
                name: /.*/,
                styles: true,
                classes: true,
                attributes: true,
              },
            ],
          },
          ...(enableImageUpload
            ? {
                image: {
                  insert: { integrations: ['upload', 'fileLibrary', 'url'] },
                },
              }
            : {}),
        }}
        data={realInitial}
        onChange={cb}
        onReady={setEditor}
        onBlur={onBlur}
      />
      {enableImageUpload ? (
        <ImageLibraryDialog
          open={libraryOpen}
          onOpenChange={setLibraryOpen}
          onSelect={(file) => insertLibraryImage(file.url)}
        />
      ) : null}
    </>
  );
}

function decodeHTML(html?: string): string {
  const el = document.createElement('textarea');
  el.innerHTML = html || '';
  return el.value;
}

function EditorClassPlugin(editor: ClassicEditor) {
  editor.ui.on('ready', () => {
    editor.ui.view.body.bodyCollectionContainer?.classList.add(
      'prose',
      'prose-accent',
      'bg-accent-1!',
    );

    if (editor.ui.view.element) {
      editor.ui.view.element.parentElement?.classList.add(
        'prose',
        'prose-accent',
        'bg-accent-1!',
      );
    }
  });

  editor.editing.view.change((writer) => {
    const root = editor.editing.view.document?.getRoot()?.parent;
    if (root) writer.addClass('prose prose-accent bg-accent-1!', root as any);
  });
}

class FileUploadAdapter {
  private xhr?: XMLHttpRequest;
  private readonly loader: FileLoader;

  constructor(loader: FileLoader) {
    this.loader = loader;
  }

  async upload() {
    const file = await this.loader.file;
    if (!file) throw new Error('Obrázek se nepodařilo načíst.');

    return new Promise<{ default: string }>((resolve, reject) => {
      const xhr = (this.xhr = new XMLHttpRequest());
      const genericError = `Obrázek ${file.name} se nepodařilo nahrát.`;

      xhr.open('POST', '/f', true);
      xhr.responseType = 'json';
      xhr.setRequestHeader('content-type', file.type || 'application/octet-stream');
      xhr.setRequestHeader('x-file-name', encodeURIComponent(file.name));
      xhr.addEventListener('error', () => reject(genericError));
      xhr.addEventListener('abort', () => reject());
      xhr.addEventListener('load', () => {
        const response = xhr.response as { error?: unknown; url?: unknown } | null;
        if (xhr.status >= 200 && xhr.status < 300 && typeof response?.url === 'string') {
          resolve({ default: response.url });
          return;
        }
        reject(typeof response?.error === 'string' ? response.error : genericError);
      });
      xhr.upload.addEventListener('progress', (event) => {
        if (event.lengthComputable) {
          this.loader.uploadTotal = event.total;
          this.loader.uploaded = event.loaded;
        }
      });
      xhr.send(file);
    });
  }

  abort() {
    this.xhr?.abort();
  }
}

function FileUploadAdapterPlugin(editor: ClassicEditor) {
  editor.plugins.get(FileRepository).createUploadAdapter = (loader) =>
    new FileUploadAdapter(loader);
}

function createFileLibraryIntegrationPlugin(openLibrary: () => void) {
  return function FileLibraryIntegrationPlugin(editor: ClassicEditor) {
    const imageInsertUI = editor.plugins.get(ImageInsertUI);
    const insertImageCommand = editor.commands.get('insertImage');
    if (!insertImageCommand) return;

    const open = () => {
      if (imageInsertUI.dropdownView) imageInsertUI.dropdownView.isOpen = false;
      openLibrary();
    };
    const createButton = (withText: boolean) => {
      const button = new ButtonView(editor.locale);
      button.set({ label: editor.locale.t('Choose from library'), withText });
      button.bind('isEnabled').to(insertImageCommand);
      button.on('execute', open);
      return button;
    };

    imageInsertUI.registerIntegration({
      name: 'fileLibrary',
      observable: insertImageCommand,
      buttonViewCreator: createButton,
      formViewCreator: () => createButton(true),
      menuBarButtonViewCreator: (isOnlyOne) => {
        const button = new MenuBarMenuListItemButtonView(editor.locale);
        button.set({
          label: editor.locale.t(isOnlyOne ? 'Image' : 'From library'),
          withText: true,
        });
        button.bind('isEnabled').to(insertImageCommand);
        button.on('execute', open);
        return button;
      },
    });
  };
}
