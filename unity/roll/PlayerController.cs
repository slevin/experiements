using UnityEngine;
using System.Collections;
using UnityEngine.UI;

public class PlayerController : MonoBehaviour {

	public Text displayText;
	public float speed;
	private int count;

	void Start () {
		count = 0;
		UpdateDisplayText ();
	}

	private void UpdateDisplayText() {
		displayText.text = "Collected: " + count;
	}

	void FixedUpdate() {
		float moveHorizontal = Input.GetAxis ("Horizontal");
		float moveVertical = Input.GetAxis ("Vertical");
		
		Vector3 movement = new Vector3 (moveHorizontal, 0.0f, moveVertical);
		
		rigidbody.AddForce (movement * speed * Time.deltaTime);
	}

	void OnTriggerEnter(Collider other) {
		if (other.gameObject.tag == "Pickup") {
			other.gameObject.SetActive(false);
			count++;
			UpdateDisplayText();
		}
	}

}

