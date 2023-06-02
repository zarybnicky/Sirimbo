import dynamic from 'next/dynamic';

export const RichTextEditor = dynamic(() => import('./RichTextEditor-client'), {
  ssr: false,
});
