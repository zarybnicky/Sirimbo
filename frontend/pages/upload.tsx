import { LoginForm } from '@app/ui/LoginForm';
import { useAuth } from '@app/ui/use-auth';
import React from 'react';
import { useDropzone } from 'react-dropzone';
import { rgbaToThumbHash, thumbHashToDataURL } from 'thumbhash';
import { AttachmentDirectoriesDocument, AttachmentsDocument, CreateAttachmentDocument } from '@app/graphql/Attachment';
import { useMutation, useQuery } from 'urql';
import { TextField } from '@app/ui/fields/text';
import { Layout } from '@/components/layout/Layout';
import { buttonCls } from '@app/ui/style';

type Image = {
  file: File;
  thumbhash: string;
  objectURL: string;
  width: number;
  height: number;
  status: 'waiting' | 'uploading' | 'error' | 'done';
};

export default function UploadPage() {
  const { user } = useAuth();
  const [newFiles, setNewFiles] = React.useState<Image[]>([]);

  const { getRootProps, getInputProps, open } = useDropzone({
    noClick: true,
    accept: {
      'image/png': ['.png'],
      'image/jpg': ['.jpg', '.jpeg'],
    },
    onDrop(acceptedFiles) {
      acceptedFiles.forEach(async (file) => {
        const objectURL = URL.createObjectURL(file);
        const image = await new Promise<HTMLImageElement>((resolve, reject) => {
          const img = new Image();
          img.onload = () => resolve(img);
          img.onerror = (...args) => reject(args);
          img.src = objectURL;
        });
        const { width, height } = image;

        const canvas = document.createElement("canvas");
        const scale = 100 / Math.max(width, height)
        canvas.width = Math.round(width * scale)
        canvas.height = Math.round(height * scale)
        const context = canvas.getContext("2d")!;
        context.drawImage(image, 0, 0, canvas.width, canvas.height)
        const pixels = context.getImageData(0, 0, canvas.width, canvas.height)

        const binaryThumbHash = rgbaToThumbHash(pixels.width, pixels.height, pixels.data)
        const thumbhash = btoa(String.fromCharCode(...binaryThumbHash))

        setNewFiles(xs => xs.concat({ file, thumbhash, objectURL, width, height, status: 'waiting' }))
      });
    },
  });

  const [directory, setDirectory] = React.useState('');
  const [{ data: existingFiles }] = useQuery({ query: AttachmentsDocument, variables: { directory }, requestPolicy: 'cache-and-network' });
  const [{ data: directories }] = useQuery({ query: AttachmentDirectoriesDocument, requestPolicy: 'cache-and-network' });
  const mutate = useMutation(CreateAttachmentDocument)[1];

  const confirm = React.useCallback(async (e: React.MouseEvent) => {
    e.preventDefault();
    setNewFiles(xs => xs.map(x => ({ ...x, status: 'uploading' })));

    await Promise.all(newFiles.map(async ({ file, width, height, thumbhash }) => {
      const objectName = [directory, `${+new Date()}-${file.name}`].filter(Boolean).join('/');
      const result = await mutate({
        input: {
          attachment: { objectName, width, height, thumbhash },
        },
      });
      const { uploadUrl } = result.data?.createAttachment?.attachment || {};
      if (uploadUrl) {
        await fetch(uploadUrl, { method: 'PUT', body: file });
        setNewFiles(xs => xs.map(x => x.file === file ? ({ ...x, status: 'done' }) : x))
      } else {
        console.log(result);
        setNewFiles(xs => xs.map(x => x.file === file ? ({ ...x, status: 'error' }) : x))
      }
    }));
    setTimeout(() => {
      setNewFiles(newFiles => {
        newFiles.forEach(file => URL.revokeObjectURL(file.objectURL));
        return []
      });
    }, 2000);
  }, [newFiles, directory, mutate]);

  React.useEffect(() => {
    return () => newFiles.forEach(file => URL.revokeObjectURL(file.objectURL));
  }, []);

  if (!user) {
    return <LoginForm />;
  }

  return (
    <Layout>
      <section className="container prose prose-accent">
        {(directories?.attachmentDirectories?.nodes || []).map(x => (
          <button key={x} className={buttonCls({ variant: directory === x ? 'primary' : 'outline'})} onClick={() => setDirectory(x || '')}>
            {x}
          </button>
        ))}

        <div className="flex gap-2 items-stretch">
          <TextField className="grow" placeholder="Složka" value={directory} onChange={e => setDirectory(e.currentTarget.value)} />
          <button type="button" className={buttonCls()} onClick={confirm} disabled={!newFiles.length}>Nahrát</button>
        </div>

        <div {...getRootProps({ className: 'dropzone' })}>
          <input {...getInputProps()} />

          <button className={buttonCls()} onClick={open}>Přidat soubory</button>

          {newFiles.map((image) => (
            <div className="flex" key={image.file.name}>
              <img src={image.objectURL} draggable={false} />
              <img width={image.width} draggable={false} height={image.height} src={thumbHashToDataURL(new Uint8Array(atob(image.thumbhash).split('').map(x => x.charCodeAt(0))))} />
            </div>
          ))}

          {(existingFiles?.attachments?.nodes || []).map(x => (
            <a className="block" target="_blank" href={x.publicUrl} key={x.objectName}>
              {x.objectName}
            </a>
          ))}
        </div>
      </section>
    </Layout>
  );
}
