extern crate sfml;

use sfml::system::Vector2f;
use sfml::window::{ContextSettings, VideoMode, event, Close};
use sfml::graphics::{RenderWindow, RenderTarget, CircleShape, Color};

fn main() {
    let mut env = SFMLEnv::new(800, 600);

    let mut circle = CircleShape::new().expect("Error cannot create ball.");
    circle.set_radius(30.);
    circle.set_fill_color(&Color::red());
    circle.set_position(&Vector2f::new(100., 100.));

    while env.win.is_open() {
        for event in env.win.events() {
            match event {
                event::Closed => env.win.close(),
                _ => { }
            }
        }

        env.win.clear(&Color::new_rgb(0, 200, 200));
        env.win.draw(&circle);
        env.win.display();
    }
}

struct SFMLEnv {
    win: RenderWindow,
    width: u32,
    height: u32
}

impl SFMLEnv {
    fn new(width: u32, height: u32) -> SFMLEnv {
        let mut window = RenderWindow::new(VideoMode::new_init(width, height, 32),
                                           "SFML Example",
                                           Close,
                                           &ContextSettings::default())
            .expect("Cannot create a new Render Window.");
        window.set_vertical_sync_enabled(true);
        SFMLEnv {
            win: window,
            width: width,
            height: height
        }
    }
}
