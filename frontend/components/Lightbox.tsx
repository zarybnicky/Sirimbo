import * as React from 'react';
import { useSwipeable } from 'react-swipeable';
import { Card, Dialog, Fade, IconButton, Typography, useTheme } from '@mui/material';
import ChevronLeft from '@mui/icons-material/ChevronLeft';
import ChevronRight from '@mui/icons-material/ChevronRight';
import { GalleryItem } from 'lib/data/use-gallery';
import { useRouter } from 'next/router';

export interface LightboxProps {
  dirHref: string;
  images: GalleryItem[];
  initial: number;
}

export const Lightbox = ({ dirHref, images, initial }: LightboxProps) => {
  const didMountRef = React.useRef(false);
  const router = useRouter();
  const theme = useTheme();
  const initialIx = images.findIndex(x => x.id == initial);
  const [selected, setSelected] = React.useState(Math.max(initialIx, 0));

  React.useEffect(() => {
    if (!didMountRef.current) {
      didMountRef.current = true;
      return;
    }
    router.replace(images[selected]!.href);
  }, [selected]);

  const prevImage = React.useCallback(() => (
    setSelected(sel => (sel + images.length - 1) % images.length)
  ), [images]);

  const nextImage = React.useCallback(() => (
    setSelected(sel => (sel + 1) % images.length)
  ), [images]);

  const handleKeyDown = React.useCallback<React.KeyboardEventHandler>(e => {
    if (e.key === "ArrowRight" || e.key === "Right" || e.key === " ") {
      nextImage();
    } else if (e.key === "ArrowLeft" || e.key === "Left") {
      prevImage();
    } else if (e.key === "Escape" || e.key === "Esc") {
      router.replace(dirHref);
    }
  }, []);

  const handlers = useSwipeable({
    onSwipedLeft: () => nextImage(),
    onSwipedRight: () => prevImage(),
  });

  if (!images.length) return null;

  return (
    <Dialog
      maxWidth="xl" open
      disableEscapeKeyDown
      onKeyDown={handleKeyDown}
      onClose={() => router.replace(dirHref)}
    >
      {images.map((image, ix) => (
        ix !== selected ? null : (
          <Fade key={image.img} in={true}>
            <div {...handlers}>
              <img src={image.img} alt={image.name} style={{
                display: 'block',
                maxWidth: '100%',
                maxHeight: `calc(100vh - 130px)`,
                objectFit: 'contain',
              }} />
              <Card variant="outlined" sx={{
                padding: theme.spacing(1),
                textAlign: 'center',
              }}>
                <Typography variant="caption">{image.name}</Typography>
              </Card>
            </div>
          </Fade>
        )
      ))}
      <IconButton onClick={nextImage} style={{ left: 0 }} sx={{
        position: 'absolute',
        top: '50%',
        transform: 'translateY(-50%)',
        background: 'rgba(255, 255, 255, 0.5)',
        boxShadow: theme.shadows[7],
        '&:hover': {
          background: 'rgba(255, 255, 255, 0.8)',
        },
      }}>
        <ChevronLeft />
      </IconButton>
      <IconButton onClick={prevImage} style={{ right: 0 }} sx={{
        position: 'absolute',
        top: '50%',
        transform: 'translateY(-50%)',
        background: 'rgba(255, 255, 255, 0.5)',
        boxShadow: theme.shadows[7],
        '&:hover': {
          background: 'rgba(255, 255, 255, 0.8)',
        },
      }}>
        <ChevronRight />
      </IconButton>
    </Dialog>
  );
};
