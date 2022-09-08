import * as React from 'react';
import { useHistory } from "react-router-dom";
import { useSwipeable } from 'react-swipeable';
import { makeStyles, Card, Dialog, Fade, IconButton, Typography } from '@material-ui/core';
import ChevronLeft from '@material-ui/icons/ChevronLeft';
import ChevronRight from '@material-ui/icons/ChevronRight';
import { GalleryItem } from '../data/use-gallery';

const useStyles = makeStyles((theme) => ({
  image: {
    display: 'block',
    maxWidth: '100%',
    maxHeight: `calc(100vh - 130px)`,
    objectFit: 'contain',
  },
  button: {
    position: 'absolute',
    top: '50%',
    transform: 'translateY(-50%)',
    background: 'rgba(255, 255, 255, 0.5)',
    boxShadow: theme.shadows[7],
    '&:hover': {
      background: 'rgba(255, 255, 255, 0.8)',
    },
  },
  titleCard: {
    padding: theme.spacing(1),
    textAlign: 'center',
  },
}));

export interface LightboxProps {
  dirHref: string;
  images: GalleryItem[];
  initial: number;
}

export const Lightbox = ({ dirHref, images, initial }: LightboxProps) => {
  const didMountRef = React.useRef(false);
  const styles = useStyles();
  const history = useHistory();
  const initialIx = images.findIndex(x => x.id == initial);
  const [selected, setSelected] = React.useState(Math.max(initialIx, 0));

  React.useEffect(() => {
    if (didMountRef.current) {
      return history.replace(images[selected].href);
    }
    didMountRef.current = true;
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
      history.replace(dirHref);
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
      onClose={() => history.replace(dirHref)}
    >
      {images.map((image, ix) => (
        ix !== selected ? null : (
          <Fade key={image.img} in={true}>
            <div {...handlers}>
              <img className={styles.image} src={image.img} alt={image.name} />
              <Card className={styles.titleCard} variant="outlined">
                <Typography variant="caption">{image.name}</Typography>
              </Card>
            </div>
          </Fade>
        )
      ))}
      <IconButton className={styles.button} onClick={nextImage} style={{ left: 0 }}>
        <ChevronLeft />
      </IconButton>
      <IconButton className={styles.button} onClick={prevImage} style={{ right: 0 }}>
        <ChevronRight />
      </IconButton>
    </Dialog>
  );
};
