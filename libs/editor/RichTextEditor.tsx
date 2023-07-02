import dynamic from 'next/dynamic';
import type { RichTextEditorProps } from './RichTextEditor-client';
import type { FieldPathByValue, FieldValues } from 'react-hook-form';

export const RichTextEditor = dynamic(() => import('./RichTextEditor-client'), {
  ssr: false,
}) as <T extends FieldValues>(props: RichTextEditorProps<T>) => JSX.Element;
