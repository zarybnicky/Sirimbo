'use client';

import LiteYouTubeEmbed from '@/ui/LiteYouTubeEmbed';
import { inputCls } from '@/ui/style';
import { youtubeVideoId } from '@/ui/outline/youtube';
import { createReactBlockSpec } from '@blocknote/react';

// Only the id is stored, never a URL or markup: a block that can hold nothing
// but a YouTube video needs no sanitising, and the embed is built here.
export const YouTubeBlock = createReactBlockSpec(
  {
    type: 'youtube',
    propSchema: {
      videoId: { default: '' },
      title: { default: '' },
    },
    content: 'none',
  },
  {
    render: ({ block, editor }) => {
      if (!block.props.videoId) {
        // Inserted from the menu without a link, so it asks for one rather than
        // sitting there as a dead block.
        return (
          <input
            className={inputCls({ className: 'my-2' })}
            placeholder="Odkaz na YouTube"
            autoFocus={editor.isEditable}
            onPaste={(event) => {
              const videoId = youtubeVideoId(event.clipboardData.getData('text'));
              if (videoId) {
                event.preventDefault();
                editor.updateBlock(block, { props: { videoId } });
              }
            }}
            onChange={(event) => {
              const videoId = youtubeVideoId(event.target.value);
              if (videoId) {
                editor.updateBlock(block, { props: { videoId } });
              }
            }}
          />
        );
      }

      return (
        <div className="my-2" data-youtube={block.props.videoId}>
          <LiteYouTubeEmbed
            id={block.props.videoId}
            title={block.props.title || 'YouTube'}
            poster="hqdefault"
            adNetwork={false}
            // A facade until it is clicked, so nothing loads from YouTube just by
            // opening the page. enablejsapi is what lets a collapsed block stop
            // the video without tearing the player down and losing its position.
            params="modestbranding=1&rel=0&enablejsapi=1"
          />
        </div>
      );
    },
  },
);
