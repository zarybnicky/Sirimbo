import React from 'react';
import { useDropzone } from 'react-dropzone';
import { rgbaToThumbHash, thumbHashToDataURL } from 'thumbhash';
import { AttachmentsDocument, CreateAttachmentDocument } from '@app/graphql/Attachment';
import { useMutation, useQuery } from 'urql';

type Image = {
  file: File;
  thumbhash: string;
  objectURL: string;
  width: number;
  height: number;
};

export const Dropzone = ({}: {}) => {
  const [files, setFiles] = React.useState<Image[]>([]);
  const { getRootProps, getInputProps } = useDropzone({
    accept: {
      'image/png': ['.png'],
      'image/jpg': ['.jpg'],
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

        setFiles(xs => xs.concat({ file, thumbhash, objectURL, width, height }))
      });
    },
  });

  const [data] = useQuery({ query: AttachmentsDocument });
  const [_1, mutate] = useMutation(CreateAttachmentDocument);
  const confirm = React.useCallback((e: React.MouseEvent) => {
    e.preventDefault();
    files.forEach(async (file) => {
      const objectName =  `${+new Date()}-${file.file.name}`;
      const result = await mutate({
        input: {
          attachment: { objectName },
        },
      });
      const { uploadUrl } = result.data?.createAttachment?.attachment || {};
      if (!uploadUrl) {
        console.log(result);
        return;
      }
      const uploadResult = await fetch(uploadUrl, {
        method: 'PUT',
        body: file.file,
      });
      console.log(uploadResult);
    });
  }, [files, mutate]);

  React.useEffect(() => {
    return () => files.forEach(file => URL.revokeObjectURL(file.objectURL));
  }, []);

  return (
    <section className="container prose">
      Existing:
      <ul>
        {(data.data?.attachments?.nodes || []).map(x => (
          <li>
            <a target="_blank" href={x.publicUrl} key={x.objectName}>{x.objectName}</a>
          </li>
        ))}
      </ul>

      <button type="button" onClick={confirm}>Nahrát</button>
      <div {...getRootProps({className: 'dropzone'})}>
        <input {...getInputProps()} />
        <p>Drag 'n' drop some files here, or click to select files</p>
      </div>
        {files.map((image) => (
          <div key={image.file.name}>
            <img src={image.objectURL} />
            <img width={image.width} height={image.height} src={thumbHashToDataURL(new Uint8Array(atob(image.thumbhash).split('').map(x => x.charCodeAt(0))))} />
          </div>
        ))}
    </section>
  );
};
