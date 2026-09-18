import StarterKit from '@tiptap/starter-kit';
import { NodeId } from './node-id.ts';
import { Tag } from './tag-node.ts';

// The outliner is deliberately narrow: nested lists, inline marks, links. No
// headings, block quotes or code — structure belongs to the outline itself.
export const outlineExtensions = [
  StarterKit.configure({
    blockquote: false,
    code: false,
    codeBlock: false,
    hardBreak: false,
    heading: false,
    horizontalRule: false,
    trailingNode: false,
  }),
  NodeId,
  Tag,
];
