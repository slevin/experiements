extern crate sfml;

use sfml::system::Vector2f;
use sfml::window::{ContextSettings, VideoMode, event, Close};
use sfml::graphics::{RenderWindow, RenderTarget, CircleShape, Color};

fn main() {
    let mut window = RenderWindow::new(VideoMode::new_init(800, 600, 32),
                                        "SFML Example",
                                        Close,
                                        &ContextSettings::default())
        .expect("Cannot create a new Render Window.");

    let mut circle = CircleShape::new().expect("Error cannot create ball.");
    circle.set_radius(30.);
    circle.set_fill_color(&Color::red());
    circle.set_position(&Vector2f::new(100., 100.));

    while window.is_open() {
        for event in window.events() {
            match event {
                event::Closed => window.close(),
                _ => { }
            }
        }

        window.clear(&Color::new_rgb(0, 200, 200));
        window.draw(&circle);
        window.display();
    }
}
